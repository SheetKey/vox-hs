{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Vox.Tree.Algorithm where

-- gox-hs
import Vox.Tree.Turtle
import Vox.Tree.Type

-- base
import Data.Fixed (mod')
import Debug.Trace
import Data.Maybe (isJust)

-- linear
import Linear hiding (angle, trace)

-- lens
import qualified Control.Lens.Getter as L
import qualified Control.Lens.Setter as L

-- optics-core
import Optics.Core

-- optics-extra
import Optics.State
import Optics.State.Operators
import Optics.At ()
import Optics.Empty

-- mtl
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class

-- vector
import qualified Data.Vector as V

-- MonadRandom
import Control.Monad.Random.Lazy

validateParameters :: Parameters -> Bool
validateParameters = undefined

stemFromDepth :: Int -> Stem
stemFromDepth sDepth = Stem
  { sCurve          = Curve V.empty
  , sParent         = Nothing
  , sOffset         = 0
  , sRadiusLimit    = -1
  , sLength         = 0
  , sRadius         = 0
  , sLengthChildMax = 0
  , sIndex          = -1
  , ..
  }

declination :: V3 Double -> Double
declination (V3 x y z) = atan2 (sqrt $ x * x + y * y) z

saacos :: Double -> Double
saacos fac =
  if fac <= -1 then pi
  else if fac >= 1 then 0
       else acos fac

mulQtQt :: Quaternion Double -> Quaternion Double -> Quaternion Double
mulQtQt (Quaternion q10 (V3 q11 q12 q13)) (Quaternion q20 (V3 q21 q22 q23)) =
  let t0 = q10 * q20 - q11 * q21 - q12 * q22 - q13 * q23
      t1 = q10 * q21 + q11 * q20 + q12 * q23 - q13 * q22
      t2 = q10 * q22 + q12 * q20 + q13 * q21 - q11 * q23
      t3 = q10 * q23 + q13 * q20 + q11 * q22 - q12 * q21
  in Quaternion t0 (V3 t1 t2 t3)

toTrackQuatZY :: V3 Double -> Quaternion Double
toTrackQuatZY v@(V3 x y z) =
  let axis = if abs x + abs y < (10 ^ (-4 :: Int))
             then V3 1 x 0
             else V3 (negate y) x 0
      co = z / 3
      q = axisAngle axis (saacos co)
      mat = fromQuaternion q
      fp = mat L.^._z
      angle = -0.5 * atan2 (negate fp L.^._x) (negate fp L.^._y)
      co' = cos angle
      si' = (sin angle) / 3
      q2 = Quaternion co' (v ^* si')
  in mulQtQt q q2

calcPointOnBezier :: Double -> CurvePoint -> CurvePoint -> V3 Double
calcPointOnBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in (invOffset ^ (3 :: Int)) *^ co1
          + 3 * (invOffset ^ (2 :: Int)) * offset *^ hr1
          + 3 * invOffset * (offset ^ (2 :: Int)) *^ hl2
          + (offset ^ (3 :: Int)) *^ co2

calcTangentToBezier :: Double -> CurvePoint -> CurvePoint -> V3 Double
calcTangentToBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in 3 * (invOffset ^ (2 :: Int)) *^ (hr1 - co1)
          + 6 * invOffset * offset *^ (hl2 - hr1)
          + 3 * (offset ^ (2 :: Int)) *^ (co2 - hl2)

calcHelixPoints :: RandomGen g => TurtleL -> Double -> Double
  -> C g r (V3 Double, V3 Double, V3 Double, V3 Double) 
calcHelixPoints turtle rad pitch = do
  spinAng <- getRandomR (0, 2 * pi)
  dir <- use $ turtle % #turtleDir
  let p0 = V3 0 (negate rad) (negate pitch / 4)
      p1 = V3 (4 * rad/ 3) (negate rad) 0
      p2 = V3 (4 * rad / 3) rad 0
      p3 = V3 0 rad (pitch / 4) 
      trf = toTrackQuatZY dir
      rotQuat = Quaternion spinAng (V3 0 0 1)
      p0' = rotate trf $ rotate rotQuat p0
      p1' = rotate trf $ rotate rotQuat p1
      p2' = rotate trf $ rotate rotQuat p2
      p3' = rotate trf $ rotate rotQuat p3
  return (p1' - p0', p2' - p0', p3' - p0', dir)

calcShapeRatio :: PShape -> Double -> C g r Double
calcShapeRatio shape ratio =
  case shape of
    Spherical -> return $ 0.2 + 0.8 * sin (pi * ratio)
    Hemispherical -> return $ 0.2 + 0.8 * sin (0.5 * pi * ratio)
    Cylindrical -> return 1
    TaperedCylindrical -> return $ 0.5 + 0.5 * ratio
    Flame -> return $ if ratio <= 0.7 then ratio / 0.7 else (1 - ratio) / 0.3
    InverseConical -> return $ 1 - 0.8 * ratio
    TendFlame -> return $ if ratio <= 0.7
                          then 0.5 + 0.5 * ratio / 0.7
                          else 0.5 + 0.5 * (1 - ratio) / 0.3
    Envelope -> if ratio < 0 || ratio > 1
         then return 0
         else do Parameters {..} <- ask
                 if ratio < 1 - pPruneWidthPeak
                   then return $ (ratio / (1 - pPruneWidthPeak)) ** pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - pPruneWidthPeak)) ** pPrunePowerLow
    Conical -> return $ 0.2 + 0.8 * ratio
      
getParent :: TreeL -> StemL -> C g r Stem
getParent tree stem = do
  p <- use $ stem % #sParent
  let Just parentIdx = p
  stems <- use $ tree % #tStems
  return $ stems V.! parentIdx

calcStemLength :: RandomGen g => TreeL -> StemL -> C g r Double
calcStemLength tree stem = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  result <- case depth of
              0 -> do -- trunk
                r <- getRandomR (-1, 1)
                treeScale <- use $ tree % #tTreeScale
                tree % #tTrunkLength <.=
                  (treeScale * ((pLength V.! 0) + r * (pLengthV V.! 0)))
              1 -> do -- first level
                parent <- getParent tree stem
                baseLength <- use $ tree % #tBaseLength
                offset <- use $ stem % #sOffset
                shapeRatio <- calcShapeRatio pShape $
                  (parent^. #sLength - offset) / (parent^. #sLength - baseLength)
                return $ (parent^. #sLength) * (parent^. #sLengthChildMax) * shapeRatio
              _ -> do
                parent <- getParent tree stem
                offset <- use $ stem % #sOffset
                return $ (parent^. #sLengthChildMax) * (parent^. #sLength - 0.7 * offset)
  return $ max 0 result

calcStemRadius :: TreeL -> StemL -> C g r Double
calcStemRadius tree stem = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  if depth == 0
    then (use $ stem % #sLength) <&> (* (pRatio * (pRadiusMod V.! 0)))
    else do parent <- getParent tree stem
            rlimit <- use $ stem % #sRadiusLimit
            length <- use $ stem % #sLength
            return $ min rlimit $ max 0.005 $
              (pRadiusMod V.! depth) * (parent^. #sRadius) *
              ((length / parent^. #sLength) ** pRatioPower)

calcCurveAngle :: RandomGen g => Int -> Int -> C g r Double
calcCurveAngle depth segInd = do
  Parameters {..} <- ask
  let curve = pCurve V.! depth
      curveBack = pCurveBack V.! depth
      curveRes = fromIntegral $ pCurveRes V.! depth
      curveV = pCurveV V.! depth
  curveAngle <- callCC $ \ break -> do
    when (curveBack == 0) $ do
      break $ curve / curveRes
    when (fromIntegral segInd < curveRes / 2) $ do
      break $ curve / (curveRes / 2)
    return $ curveBack / (curveRes / 2)
  r <- getRandomR (-1, 1)
  return $ curveAngle + r * (curveV / curveRes)

calcDownAngle :: RandomGen g => StemL -> Double -> C g r Double
calcDownAngle stem stemOffset = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let dp1 = min (depth + 1) pMaxLevel
  if pDownAngleV V.! dp1 >= 0
    then do r <- getRandomR (-1, 1)
            return $ (pDownAngle V.! dp1) + r * (pDownAngleV V.! dp1)
    else do length <- use $ stem % #sLength
            shapeRatio <- calcShapeRatio Spherical $
                          (length - stemOffset) / (length * (1 - pBaseSize V.! depth))
            let dAngle = (pDownAngle V.! dp1) + (pDownAngleV V.! dp1) * (1 - 2 * shapeRatio)
            r <- getRandomR (-1, 1)
            return $ dAngle + r * abs (dAngle * 0.1)

calcRotateAngle :: RandomGen g => Int -> Double -> C g r Double
calcRotateAngle depth prevAngle = do
  Parameters {..} <- ask
  if pRotate V.! depth >= 0
    then do r <- getRandomR (-1, 1)
            return $ mod' (prevAngle + (pRotate V.! depth) + r * (pRotateV V.! depth)) (2 * pi)
    else do r <- getRandomR (-1, 1)
            return $ prevAngle * (pi + (pRotate V.! depth) + r * (pRotateV V.! depth))

calcLeafCount :: TreeL -> StemL -> C g r Double
calcLeafCount tree stem = do
  Parameters {..} <- ask
  if pLeafBlosNum >= 0
    then do treeScale <- use $ tree % #tTreeScale
            let leaves = fromIntegral pLeafBlosNum * treeScale / pGScale
            parent <- getParent tree stem
            length <- use $ stem % #sLength
            return $ leaves * (length / (parent^. #sLengthChildMax * parent^. #sLength))
    else return $ fromIntegral pLeafBlosNum

calcBranchCount :: RandomGen g => TreeL -> StemL -> C g r Double
calcBranchCount tree stem = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  result <- callCC $ \ break -> do
    let dp1 = min (depth + 1) pMaxLevel
        branches = fromIntegral $ pBranches V.! dp1
    when (depth == 0) $ do
      r <- getRandomR (0, 1)
      break $ branches * (r * 0.2 + 0.9)
    when (branches < 0) $
      break branches
    parent <- getParent tree stem
    when (depth == 1) $ do
      length <- use $ stem % #sLength
      break $ branches *
        (0.2 + 0.8 * (length / parent^. #sLength) / parent^. #sLengthChildMax)
    offset <- use $ stem % #sOffset
    return $ branches * (1 - 0.5 * offset / (parent^. #sLength))
  return $ result / (1 - pBaseSize V.! depth)

calcRadiusAtOffset :: StemL -> Double -> C g r Double
calcRadiusAtOffset stem z1 = do
  Parameters {..} <- ask
  nTaper <- use (stem % #sDepth) <&> (pTaper V.!)
  unitTaper <- callCC $ \ break -> do
    when (nTaper < 1) $
      break nTaper
    when (nTaper < 2) $
      break $ 2 - nTaper
    return 0
  taper <- use (stem % #sRadius) <&> (* (1 - unitTaper * z1))
  radius <- callCC $ \ break -> do
    when (nTaper < 1) $
      break taper
    length <- use $ stem % #sLength
    let z2 = (1 - z1) * length
        depth = if (nTaper < 2 || z2 < taper) then 1 else nTaper - 2
        z3 = if nTaper < 2 then z2 else abs $ z2 - 2 * taper *
                                        ((fromIntegral :: Int -> Double) . truncate)
                                        (z2 / (2 * taper) + 0.5)
    when (nTaper < 2 && z3 >= taper) $
      break taper
    return $ (1 - depth) * taper + depth *
      sqrt ((taper ^ (2 :: Int)) - ((z3 - taper) ^ (2 :: Int)))
  d <- use $ stem % #sDepth
  if d == 0
    then let yVal = max 0 (1 - 8 * z1)
             flare = pFlare * (100 ** yVal) / 100 + 1
         in  return $ radius * flare
    else return radius

blankBP :: CurvePoint
blankBP = CurvePoint (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) 0

increaseBezierPointRes :: StemL -> Int -> Int -> C g r ()
increaseBezierPointRes stem segInd pointsPerSeg = do
  when (pointsPerSeg <= 2) $ error "'pointsPerSeg' must be greater than '2'."
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  curve <- use $ stem % #sCurve % #bezierPoints
  let curveRes = fromIntegral $ pCurveRes V.! depth
      curveNumPoints = V.length curve
      segEndPoint = curve V.! (curveNumPoints - 1)
      segStartPoint = curve V.! (curveNumPoints - 2)
  (loop, k) <- label (0 :: Int)
  when (k < pointsPerSeg) $ do
    let offset = fromIntegral k / (fromIntegral pointsPerSeg - 1)
    -- increase the size of the curve vector if needed
    when (k > 1) $ 
      stem % #sCurve % #bezierPoints %= (`V.snoc` blankBP)
    -- set values of new bp point
    when (k == pointsPerSeg - 1) $
      stem % #sCurve % #bezierPoints % (ix $ curveNumPoints - 2 + k) .= segEndPoint
    when (0 < k && k < pointsPerSeg - 1) $ do
      let co = calcPointOnBezier offset segStartPoint segEndPoint
          tangent = normalize $ calcTangentToBezier offset segStartPoint segEndPoint
          dirVecMag = norm $ segEndPoint^. #bpHandleLeft - segStartPoint^. #bpControl
          hl = co - tangent ^* dirVecMag
          hr = co + tangent ^* dirVecMag
      stem % #sCurve % #bezierPoints % (ix $ curveNumPoints - 2 + k) .= (CurvePoint co hl hr 0)
    -- set radius of new bp point
    radiusAtOffset <- calcRadiusAtOffset stem $ (offset + fromIntegral segInd - 1) / curveRes
    stem % #sCurve % #bezierPoints % (ix $ curveNumPoints - 2 + k) % #bpRadius .= radiusAtOffset
    -- loop
    loop (k + 1)

pointInside :: TreeL -> V3 Double -> C g r Bool
pointInside tree (V3 x y z) = do
  Parameters {..} <- ask
  treeScale <- use $ tree % #tTreeScale
  let dist = sqrt $ (x ^ (2 :: Int)) + (y ^ (2 :: Int))
      ratio = (treeScale - z) / (treeScale * (1 - (pBaseSize V.! 0)))
  shapeRatio <- calcShapeRatio Envelope ratio
  return $ (dist / treeScale) < (pPruneWidth * shapeRatio)

makeBranchPosTurtle :: Turtle -> Double -> CurvePoint -> CurvePoint -> Double -> (Turtle, Turtle)
makeBranchPosTurtle dirTurtle offset startPoint endPoint radiusLimit =
  let pos = calcPointOnBezier offset startPoint endPoint
      dirTurtle' = dirTurtle { turtlePos = pos }
      branchPosTurtle = move radiusLimit $ pitchDown (pi / 2) dirTurtle'
  in (dirTurtle', branchPosTurtle)

makeBranchDirTurtle :: Turtle -> Bool -> Double -> CurvePoint -> CurvePoint -> Turtle
makeBranchDirTurtle turtle isHelix offset startPoint endPoint =
  let tangent = normalize $ calcTangentToBezier offset startPoint endPoint
      right = if isHelix
              then let tanD = normalize $ calcTangentToBezier (offset + 0.0001) startPoint endPoint
                   in tangent `cross` tanD
              else (turtleDir turtle `cross` turtleRight turtle) `cross` tangent
  in Turtle tangent (V3 0 0 0) right

applyTropism :: V3 Double -> Turtle -> Turtle
applyTropism tropismVector turtle =
  let hcrosst = turtleDir turtle `cross` tropismVector
      alpha = pi * (10 * norm hcrosst) / 180
      nhcrosst = normalize hcrosst
      dir = normalize $ rotate (axisAngle nhcrosst alpha) (turtleDir turtle)
      right = normalize $ rotate (axisAngle nhcrosst alpha) (turtleRight turtle)
  in turtle { turtleDir = dir, turtleRight = right }

scaleBezierHandlesForFlare :: StemL -> Int -> C g r ()
scaleBezierHandlesForFlare stem maxPointsPerSeg = do
  stem % #sCurve % #bezierPoints % traversed %=
    (\ CurvePoint {..} -> CurvePoint
      { bpHandleLeft = bpControl
        + (bpHandleLeft - bpControl) ^/ fromIntegral maxPointsPerSeg
      , bpHandleRight = bpControl
        + (bpHandleRight - bpControl) ^/ fromIntegral maxPointsPerSeg
      , ..})

pointsForFloorSplit :: RandomGen g => TreeL -> C g r (V.Vector (V3 Double, Double))
pointsForFloorSplit tree = do
  Parameters {..} <- ask
  tree % #tTreeScale .= pGScale + pGScaleV
  let dummyStem = stemFromDepth 0
      branches = pBranches V.! 0
  (dummyStemL, freeDummyStem) <- newVar "dummyStem" dummyStem
  l <- calcStemLength tree dummyStemL 
  dummyStemL % #sLength .= l
  rad <- calcStemRadius tree dummyStemL <&> (* 2.5)
  freeDummyStem
  (loop, (points, k)) <- label (V.empty, 0 :: Int)
  when (k < branches) $ do
    r <- getRandomR (0, 1)
    let dis = sqrt $ r * (fromIntegral branches) / 2.5 * pGScale * pRatio
    theta <- getRandomR (0, 2 * pi)
    let pos = V3 (dis * cos theta) (dis * sin theta) 0
    if V.all (\(p, _) -> norm (pos - p) < rad) points
      then loop (points `V.snoc` (pos, theta), k + 1)
      else loop (points, k)
  return points

treeBasePoints :: RandomGen g => TreeL -> C g r (V.Vector (V3 Double, Double))
treeBasePoints tree = do
  Parameters {..} <- ask
  if pBranches V.! 0 > 0
    then pointsForFloorSplit tree <&> (traversed % _2 %~ (\ a -> a - (pi / 2)))
    else do r <- getRandomR (0, 2 * pi)
            return $ V.singleton (V3 0 0 0, r)
    
calcHelixParameters :: RandomGen g => StemL -> TurtleL
  -> C g r (V3 Double, V3 Double, V3 Double, V3 Double)
calcHelixParameters stem turtle = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  length <- use $ stem % #sLength
  r1 <- getRandomR (0.8, 1.2)
  r2 <- getRandomR (0.8, 1.2)
  let tanAng = tan $ (pi / 2) - abs (pCurveV V.! depth)
      helPitch = 2 * length / (fromIntegral $ pCurveRes V.! depth) * r1
      helRadius = 3 * helPitch / (16 * tanAng) * r2
  if depth > 1
    then turtle %= (applyTropism pTropism)
    else turtle %= (applyTropism (pTropism & (L..~)_z 0))
  calcHelixPoints turtle helRadius helPitch

testStemHelix :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> C g r Bool
testStemHelix tree stem turtle start = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let curveRes = pCurveRes V.! depth
  (_, _, helP2, helAxis) <- calcHelixParameters stem turtle
  (loop, (segInd, prevHelPnt)) <- label (start, V3 0 0 0) 
  when (segInd < curveRes + 1) $ do
    when (segInd == 1) $
      turtle % #turtlePos %= (+ helP2)
    when (segInd > 1) $
      let helP2' = rotate (axisAngle helAxis $ (fromIntegral segInd - 1) * pi) helP2
      in turtle % #turtlePos .= (prevHelPnt + helP2')
    nextPrevHelPnt <- use $ turtle % #turtlePos
    loop (segInd + 1, nextPrevHelPnt)
  pos <- use $ turtle % #turtlePos
  pointInside tree pos

testStemRegular :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double
  -> C g r Bool
testStemRegular tree stem turtle start splitCorrAngle cloneProb = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let curveRes = pCurveRes V.! depth
      segSplits = pSegSplits V.! depth
      baseSegInd = ceiling $ (pBaseSize V.! 0) * (fromIntegral $ pCurveRes V.! 0)
  segLength <- use (stem % #sLength) <&> (/ fromIntegral curveRes)
  callCC $ \ break -> do
    (cpL, freecp) <- newVarWithInt 0 ("cp" ++ show depth) cloneProb
    (numOfSplits, freeNumOfSplits) <- newVarWithInt 0 ("numOfSplits" ++ show depth) (0 :: Int)
    (splitCorrAngleL, freeSCA) <- newVarWithInt 0 ("splitCorrAngle" ++ show depth) splitCorrAngle
    (loop, segInd) <- label start
    when (segInd < curveRes + 1) $ do
      let remainingSegs = curveRes + 1 - segInd
      when (segInd > start) $ do
        turtle %= (move segLength)
        tPos <- use (turtle % #turtlePos)
        isInside <- pointInside tree tPos
        when (not (depth == 0 && start < baseSegInd) && not isInside) $
          break False
        numOfSplits .= 0
        when (pBaseSplits > 0 && depth == 0 && segInd == baseSegInd) $ do
          r <- getRandomR (0 :: Double, 1)
          numOfSplits .= (truncate $ r * (fromIntegral pBaseSplits + 0.5))
        when (segSplits > 0 && segInd < curveRes && (depth > 0 || segInd > baseSegInd)) $ do
          r <- getRandomR (0, 1)
          cp <- use cpL
          when (r <= cp) $ do
            splitNumError <- use $ tree % #tSplitNumError
            nos <- numOfSplits <.= (truncate $ segSplits + splitNumError V.! depth)
            tree % #tSplitNumError % (ix depth) %= (flip (-) $ (fromIntegral nos) - segSplits)
            cpL %= (/ (fromIntegral nos + 1))

        nos <- use numOfSplits

        -- TODO: Use maybe instead of undefined, although it should be fine
        cloneStemSplits tree stem turtle depth segInd baseSegInd remainingSegs curveRes
          nos splitCorrAngleL undefined undefined True

        applyStemTropism turtle depth

      loop (segInd + 1)

    freecp
    freeNumOfSplits
    freeSCA
    tPos <- use $ turtle % #turtlePos
    pointInside tree tPos
        
testStem :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double -> C g r Bool
testStem tree stem turtle start splitCorrAngle cloneProb = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  if pCurveV V.! depth < 0
    then testStemHelix tree stem turtle start
    else testStemRegular tree stem turtle start splitCorrAngle cloneProb

setUpBranch :: RandomGen g => StemL -> TurtleL -> BranchMode -> Double -> CurvePoint
  -> CurvePoint -> Double -> Int -> DoubleL -> Int -> C g r (Turtle, Turtle, Double, Double)
setUpBranch stem turtle branchMode offset startPoint endPoint stemOffset branchInd prevRotAngle branchesInGroup
  = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  turt <- use turtle
  let dp1 = min pMaxLevel (depth + 1)
      bdt = makeBranchDirTurtle turt (pCurveV V.! depth < 0) offset startPoint endPoint
  (radiusLimit, branchDirTurtle) <- case branchMode of
    Fan -> do tAngle <- if branchesInGroup == 1
                then return 0
                else do r <- getRandomR (-1, 1)
                        return $ ((pRotate V.! dp1) *
                                  ((fromIntegral branchInd / (fromIntegral branchesInGroup - 1))
                                   - 1 / 2)) + r * (pRotateV V.! dp1)
              return (0, turnRight tAngle bdt)
    Whorled -> do r <- getRandomR (-1, 1)
                  pra <- use prevRotAngle
                  let rAngle = pra +
                        (2 * pi * fromIntegral branchInd / fromIntegral branchesInGroup)
                        + r  * (pRotateV V.! dp1)
                  l <- use $ stem % #sLength
                  rl <- calcRadiusAtOffset stem $ stemOffset / l
                  return (rl, rollRight rAngle bdt)
    AltOpp -> do pra <- use prevRotAngle
                 rAngle <- calcRotateAngle dp1 pra
                 if (pRotate V.! dp1) >= 0
                   then prevRotAngle .= rAngle
                   else prevRotAngle .= negate pra
                 l <- use $ stem % #sLength
                 rl <- calcRadiusAtOffset stem $ stemOffset / l
                 return (rl, rollRight rAngle bdt)
  let (dirTurtle, posTurtle) = makeBranchPosTurtle
                               branchDirTurtle offset startPoint endPoint radiusLimit
  dAngle <- calcDownAngle stem stemOffset
  return (posTurtle, pitchDown dAngle dirTurtle, radiusLimit, stemOffset)

makeBranches :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> DoubleL -> Bool
  -> C g r ()
makeBranches tree stem turtle segInd branchesOnSeg prevRotAngle isLeaves = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let dp1 = min (depth + 1) pMaxLevel
  bp <- use $ stem % #sCurve % #bezierPoints
  let curveNumPoints = V.length bp
      endPoint = bp V.! (curveNumPoints - 1)
      startPoint = bp V.! (curveNumPoints - 2)
  (branchesArray, freeBranchesArray) <- newVar "branchesArray" V.empty
  if branchesOnSeg < 0 -- fan branches
    then do (loop, branchInd) <- label (0 :: Int)
            when (branchInd < abs branchesOnSeg) $ do
              newBranch <- setUpBranch stem turtle 
                           Fan 1 startPoint endPoint 1 branchInd prevRotAngle (abs branchesOnSeg)
              branchesArray %= (`V.snoc` newBranch)
              loop (branchInd + 1)
    else do length <- use $ stem % #sLength
            let baseLength = length * (pBaseSize V.! depth)
                branchDist = pBranchDist V.! dp1
                curveRes = pCurveRes V.! depth
            if branchDist > 1 -- whorled branches
              then do let numOfWhorls = truncate (fromIntegral branchesOnSeg / (branchDist + 1))
                          branchesPerWhorl = branchDist + 1
                      (branchWhorlError, freeBranchWhorlError) <- newVar
                                                                  "branchWhorlError" (0 :: Double)
                      (loop, whorlNum) <- label (0 :: Int)
                      when (whorlNum < numOfWhorls) $ do
                        let offset = min 1 $ max 0 $
                                     fromIntegral whorlNum / fromIntegral numOfWhorls
                            stemOffset = length *
                              (((fromIntegral segInd - 1) + offset) / fromIntegral curveRes)
                        when (stemOffset > baseLength) $ do
                          bwe <- use branchWhorlError
                          let branchesThisWhorl = truncate $ branchesPerWhorl + bwe
                          branchWhorlError %=
                            (\ a -> a - (fromIntegral branchesThisWhorl - branchesPerWhorl))
                          (loopB, branchInd) <- label (0 :: Int)
                          when (branchInd < branchesThisWhorl) $ do
                            newBranch <- setUpBranch stem turtle Whorled offset startPoint
                                         endPoint stemOffset branchInd
                                         prevRotAngle branchesThisWhorl
                            branchesArray %= (`V.snoc` newBranch)
                            loopB (branchInd + 1)
                        prevRotAngle %= (+ (pRotate V.! dp1))
                        loop (whorlNum + 1)
                      freeBranchWhorlError
              else -- alternating or opposite branches
              do (loop, branchInd) <- label (0 :: Int)
                 when (branchInd < branchesOnSeg) $ do
                   let offset = if branchInd `mod` 2 == 0
                                then min 1 $ max 0 $
                                     fromIntegral branchInd / fromIntegral branchesOnSeg
                                else min 1 $ max 0 $
                                     (fromIntegral branchInd - branchDist)
                                     / fromIntegral branchesOnSeg
                       stemOffset = length * 
                                    (((fromIntegral segInd - 1) + offset) / fromIntegral curveRes)
                   when (stemOffset > baseLength) $ do
                     newBranch <- setUpBranch stem turtle AltOpp offset startPoint endPoint
                                  stemOffset branchInd prevRotAngle 0
                     branchesArray %= (`V.snoc` newBranch)
                   loop (branchInd + 1)
  if isLeaves
    then error "leaves not yet supported"
    else do newBranches <- use branchesArray
            freeBranchesArray
            i <- use $ stem % #sIndex
            V.forM_ newBranches $ \ (posTur, dirTur, rad, bOffset) -> do
              let s = (stemFromDepth dp1)
                      { sParent = Just i, sOffset = bOffset, sRadiusLimit = rad }
              newStem <- newStemVar tree s
              (newTurt, freeNewTurt) <- newVar ("newTurt" ++ show depth) dirTur
              makeStem tree newStem newTurt 0 0 1 1 (Just posTur) Nothing
              freeNewTurt
              
makeClones :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double -> Double -> Int
  -> Double -> Double -> Bool -> C g r ()
makeClones tree stem turtle segInd splitCorrAngle numBranchesFactor cloneProb numOfSplits
  splAngle sprAngle isBaseSplit = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let usingDirectSplit = pSplitAngle V.! depth < 0
      stemDepth = pSplitAngleV V.! depth
  when (not isBaseSplit && numOfSplits > 2 && usingDirectSplit) $
    error "Only splitting up to 3 branches is supported."
  (loop, splitIndex) <- label (0 :: Int)
  when (splitIndex < numOfSplits) $ do
    t <- use turtle
    (nTurtle, freeNTurtle) <- newVarWithInt 0
                              ("," ++ "nTurtle" ++ show depth ++ "," ++ show splitIndex) t
    nTurtle %= (pitchDown $ splAngle / 2)
    effSprAngle <- if isBaseSplit && not usingDirectSplit
                   then do r <- getRandomR (-1, 1)
                           return $
                             (fromIntegral splitIndex + 1)
                             * (2 * pi / (fromIntegral numOfSplits + 1))
                             + r * stemDepth
                   else if splitIndex == 0
                        then return $ sprAngle / 2
                        else return $ negate $ sprAngle / 2
    if usingDirectSplit
      then nTurtle %= (turnLeft effSprAngle)
      else do let quat = axisAngle (V3 0 0 1) effSprAngle
              nTurtle % #turtleDir %= (rotate quat)
              turtle % #turtleDir %= normalize
              nTurtle % #turtleRight %= (rotate quat)
              turtle % #turtleRight %= normalize
    s <- use stem
    newStem <- newStemVar tree s
    newStem % #sCurve .= (Curve V.empty)
    let cloned = if stemDepth >= 0
                 then Just t
                 else Nothing
    makeStem tree newStem nTurtle segInd splitCorrAngle numBranchesFactor cloneProb Nothing cloned
    freeNTurtle
    loop $ splitIndex + 1

-- calc and set the length and radius for stem (only for non-clones: assumes 'start == 0')
setupStemLR :: RandomGen g => TreeL -> StemL -> Int -> Int -> C g r ()
setupStemLR tree stem depth dp1 = do
  Parameters {..} <- ask
  r <- getRandomR (-1, 1)
  stem % #sLengthChildMax .= ((pLength V.! dp1) + r * (pLengthV V.! dp1))
  l <- calcStemLength tree stem
  stem % #sLength .= l 
  (stem % #sRadius .=) =<< calcStemRadius tree stem
  when (depth == 0) $
    tree % #tBaseLength .= (l * (pBaseSize V.! 0))

-- corrects the main turtles position if there is a posCorrTurtle
correctTurtlePos :: StemL -> TurtleL -> Maybe Turtle -> C g r ()
correctTurtlePos stem turtle posCorrTurtle = 
  case posCorrTurtle of
    Nothing -> return ()
    Just pct -> do
      sr <- use $ stem % #sRadius
      srl <- use $ stem % #sRadiusLimit
      let newPos = negate $ min sr srl
          newPCT = move newPos pct
      turtle % #turtlePos .= (turtlePos newPCT)

-- only for non-clones
applyPruning :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double
  -> (() -> C g r ()) -> C g r ()
applyPruning tree stem turtle start splitCorrAngle cloneProb returnEarly = do
  Parameters {..} <- ask
  startLength <- use $ stem % #sLength
  rState <- getRandomState
  splitErrState <- use $ tree % #tSplitNumError
  
  t <- use turtle
  (testTurtle, freeTestTurtle) <- newVarWithInt 0 "testTurtle" t
  inPruningEnvelopeInit <- testStem tree stem testTurtle start splitCorrAngle cloneProb
  callCC $ \ break -> do
    (loop, inPruningEnvelope) <- label inPruningEnvelopeInit
    when (not inPruningEnvelope) $ do
      stem % #sLength %= (* 0.9)
      l <- use $ stem % #sLength
      when (l < 0.15 * startLength) $ do
        if pPruneRatio < 1
          then do stem % #sLength .= 0
                  break ()
          else returnEarly ()
      setRandomState rState
      tree % #tSplitNumError .= splitErrState

      testTurtle .= t
      inPruningEnvelope' <- testStem tree stem testTurtle start splitCorrAngle cloneProb
      loop inPruningEnvelope'

  freeTestTurtle
  fittingLength <- use $ stem % #sLength
  stem % #sLength .= (startLength * (1 - pPruneRatio) + fittingLength * pPruneRatio)
  (stem % #sRadius .=) =<< calcStemRadius tree stem
  setRandomState rState
  tree % #tSplitNumError .= splitErrState

-- rename: initLeafBranchCounts
leafBranchCounts :: RandomGen g => TreeL -> StemL -> Int -> Int -> Int -> Double
  -> String -> C g r (DoubleL, C g r (), DoubleL, C g r ())
leafBranchCounts tree stem start depth curveRes numBranchesFactor nameStr = do
  Parameters {..} <- ask
  (leafCount, freeLeafCount) <- newVarWithInt 0
                                ("leafCount" ++ show depth ++ nameStr) (0 :: Double)
  (branchCount, freeBranchCount) <- newVarWithInt 0
                                    ("branchCount" ++ show depth ++ nameStr) (0 :: Double)
  if depth == pLevels - 1 && depth > 0 && pLeafBlosNum /= 0
    then do (leafCount .=) =<< calcLeafCount tree stem
            leafCount %= (* (1 - (fromIntegral start / fromIntegral curveRes)))
    else do (branchCount .=) =<< calcBranchCount tree stem
            branchCount %= (* (1 - (fromIntegral start / fromIntegral curveRes)))
            branchCount %= (* numBranchesFactor)
  return (leafCount, freeLeafCount, branchCount, freeBranchCount)

newPrevRotAngle :: RandomGen g => Int -> Int -> String -> C g r (DoubleL, C g r ())
newPrevRotAngle depth dp1 nameStr = do
  Parameters {..} <- ask
  (prevRotAngle, freePRA) <- newVarWithInt 0
                             ("prevRotAngle" ++ show depth ++ nameStr) (0 :: Double)
  if (pRotate V.! dp1) >= 0
    then (prevRotAngle .=) =<< getRandomR (0, 2 * pi)
    else prevRotAngle .= (pi / 180)
  return (prevRotAngle, freePRA)

calcNumOfSplits :: RandomGen g => TreeL -> Int -> Int -> Int -> Double -> Int -> DoubleL -> DoubleL
  -> DoubleL -> C g r Int
calcNumOfSplits tree depth segInd baseSegInd segSplits curveRes cloneProbL numBranchesFactorL branchCount = do
  Parameters {..} <- ask
  if abs pBaseSplits > 0 && depth == 0 && segInd == baseSegInd
    then if pBaseSplits < 0
         then do r <- getRandomR (0, 1 :: Double)
                 return $ truncate $ r * ((fromIntegral $ abs pBaseSplits) + 0.5)
         else return pBaseSplits
    else if segSplits > 0 && segInd < curveRes && (depth > 0 || segInd > baseSegInd)
         then do r <- getRandomR (0, 1)
                 cp <- use cloneProbL
                 if r <= cp
                   then do splitNumErr <- use $ tree % #tSplitNumError % (unsafeVectorLens depth)
                           let nos = truncate $ segSplits + splitNumErr
                           tree % #tSplitNumError % (ix depth) %=
                             (\ a -> a - (fromIntegral nos - segSplits))

                           cloneProbL %= (/ (fromIntegral nos + 1))
                           numBranchesFactorL %= (/ (fromIntegral nos + 1))
                           numBranchesFactorL %= (max 0.8)

                           f <- use numBranchesFactorL
                           branchCount %= (* f)
                           return nos
                   else return 0
         else return 0

addSegBranches :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> DoubleL -> DoubleL
  -> DoubleL -> C g r ()
addSegBranches tree stem turtle segInd curveRes branchCount branchNumError prevRotAngle = do
  bc <- use branchCount
  branchesOnSeg <- if bc < 0
                   then if segInd == curveRes
                        then return $ truncate bc
                        else return 0
                   else do bne <- use branchNumError
                           let fBranchesOnSeg = bc / fromIntegral curveRes
                               bos = truncate $ fBranchesOnSeg + bne
                           branchNumError %=
                             (\ a -> a - (fromIntegral bos - fBranchesOnSeg))
                           return bos
  when (abs branchesOnSeg > 0) $
    makeBranches tree stem turtle segInd branchesOnSeg prevRotAngle False

addSegLeaves :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> Int -> DoubleL
  -> DoubleL -> DoubleL -> C g r ()
addSegLeaves tree stem turtle depth segInd curveRes leafCount leafNumError prevRotAngle = do
  lc <- use leafCount
  when (abs lc > 0 && depth > 0) $ do
    leavesOnSeg <- if lc < 0
                   then if segInd == curveRes
                        then return $ truncate lc
                        else return 0
                   else do lne <- use leafNumError
                           let fLeavesOnSeg = lc / fromIntegral curveRes
                               los = truncate $ fLeavesOnSeg + lne
                           leafNumError %=
                             (\ a -> a - (fromIntegral los - fLeavesOnSeg))
                           return los
    when (abs leavesOnSeg > 0) $ 
      traceM "makeLeaves being called but not yet implemented."
      -- makeLeaves tree stem turtle segInd leavesOnSeg prevRotAngle

addSegBranchesOrLeaves :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> Int 
  -> DoubleL -> DoubleL -> DoubleL -> DoubleL -> DoubleL -> C g r ()
addSegBranchesOrLeaves tree stem turtle depth segInd curveRes branchCount branchNumError
  leafCount leafNumError prevRotAngle = do
  Parameters {..} <- ask
  rState <- getRandomState
  bc <- use branchCount
  if abs bc > 0 && depth < pLevels - 1
    then addSegBranches tree stem turtle segInd curveRes branchCount branchNumError prevRotAngle
    else addSegLeaves tree stem turtle depth segInd curveRes leafCount leafNumError prevRotAngle
  setRandomState rState

cloneStemSplits :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> Int -> Int -> Int
  -> Int -> DoubleL -> DoubleL -> DoubleL -> Bool -> C g r ()
cloneStemSplits tree stem turtle depth segInd baseSegInd remainingSegs curveRes numOfSplits
  splitCorrAngleL numBranchesFactorL cloneProbL isTestStem = do
  Parameters {..} <- ask
  if numOfSplits > 0
    then do let isBaseSplit = pBaseSplits > 0 && depth == 0 && segInd == baseSegInd
                usingDirectSplit = (pSplitAngle V.! depth) < 0
            (sprAngle, freesprAngle) <- newVar "sprAngle" 0
            (splAngle, freesplAngle) <- newVar "splAngle" 0
            if usingDirectSplit
              then do r <- getRandomR (-1, 1)
                      sprAngle .= abs ((pSplitAngle V.! depth)
                                        + r * (pSplitAngleV V.! depth))
                      splitCorrAngleL .= 0
              else do dec <- use (turtle % #turtleDir) <&> declination
                      r <- getRandomR (-1, 1)
                      spl <- splAngle <.= max 0 ((pSplitAngle V.! depth)
                                                  + r * (pSplitAngleV V.! depth) - dec)
                      splitCorrAngleL .= spl / fromIntegral remainingSegs
                      r2 <- getRandomR (0, 1)
                      sprAngle .= negate
                        ((pi / 9) + 0.75 * ((pi / 6) + abs(dec - (pi / 2)) * (r2 ^ (2 :: Int))))

            spl <- use splAngle
            spr <- use sprAngle
            freesprAngle
            freesplAngle

            when (not isTestStem) $ do
              rState <- getRandomState
              sca <- use splitCorrAngleL
              nbf <- use numBranchesFactorL
              cp <- use cloneProbL
              makeClones tree stem turtle segInd sca nbf cp numOfSplits spl spr isBaseSplit
              setRandomState rState
            
            turtle %= (pitchDown (spl / 2))

            when (not isBaseSplit && numOfSplits == 1) $ 
              if usingDirectSplit
              then turtle %= (turnRight (spr / 2))
              else do turtle % #turtleDir %=
                        (normalize . (rotate (axisAngle (V3 0 0 1) (negate spr / 2))))
                      turtle % #turtleRight %=
                        (normalize . (rotate (axisAngle (V3 0 0 1) (negate spr / 2))))

    else do r <- getRandomR (-1, 1)
            turtle %= (turnLeft $ r * (pBendV V.! depth) / fromIntegral curveRes)
            curveAngle <- calcCurveAngle depth segInd
            sca <- use splitCorrAngleL
            turtle %= (pitchDown $ curveAngle - sca)

applyStemTropism :: TurtleL -> Int -> C g r ()
applyStemTropism turtle depth = do
  Parameters {..} <- ask
  if depth > 1
    then turtle %= (applyTropism pTropism)
    else turtle %= (applyTropism (pTropism & (L..~)_z 0))

makeStemHelix :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double -> Double 
  -> Maybe Turtle -> Maybe Turtle -> C g r ()
makeStemHelix tree stem turtle start splitCorrAngle numBranchesFactor cloneProb posCorrTurtle clonedTurtle = callCC $ \ returnEarly -> do
  whenM (use (stem % #sRadiusLimit) <&> (\ a -> 0 <= a && a < 0.001)) $
    returnEarly ()

  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let dp1 = min (depth + 1) pMaxLevel

  when (start == 0) $ do
    setupStemLR tree stem depth dp1

  correctTurtlePos stem turtle posCorrTurtle

  case (clonedTurtle, pPruneRatio > 0) of
    (Nothing, True) -> applyPruning tree stem turtle start splitCorrAngle cloneProb returnEarly
    _               -> return ()

  let curveRes = pCurveRes V.! depth
      nameStr = if isJust clonedTurtle then "c" else ""

  (leafCount, freeLeafCount, branchCount, freeBranchCount) <-
    leafBranchCounts tree stem start depth curveRes numBranchesFactor nameStr
            
  let maxPointsPerSeg = (ceiling :: Double -> Int) $ max 1 $ 1 / fromIntegral curveRes

  (branchNumError, freeBNE) <- newVarWithInt 0
                               ("branchNumError" ++ show depth ++ nameStr) (0 :: Double)
  (leafNumError, freeLNE) <- newVarWithInt 0
                             ("leafNumError" ++ show depth ++ nameStr) (0 :: Double)

  (prevRotAngle, freePRA) <- newPrevRotAngle depth dp1 nameStr

  (helP0, helP1, helP2, helAxis) <- calcHelixParameters stem turtle

  let pointsPerSeg = if depth == 0 || (pTaper V.! depth) > 1
                     then maxPointsPerSeg
                     else 2

  -- MOVE THESE TWO TEST TO THE CALLSIGHT OF THIS FUNC
  -- MY ADDITION: CHECK IF NEEDED / CORRECT ASSERTION
  whenM (use (stem % #sCurve % #bezierPoints) <&> (not . V.null)) $
    error "Expected length of stem curve to be 0."

  -- MY ADDITION: should be valid: makeStem is called with start /= 0 only by makeClones
  -- but makeClones is never called for helix branches. Thus a helix branch makeStem is only
  -- called initially or by makeBranches. In either case start == 0.
  when (start /= 0) $
    error "Helix branches expect 'start == 0'."

  (loop, segInd) <- label (0 :: Int)
  when (segInd < curveRes + 1) $ do
    pos <- use $ turtle % #turtlePos
    if segInd == 0
      then do newPoint <- newBP stem
              newPoint % #bpControl .= pos
              newPoint % #bpHandleRight .= (helP0 + pos)
              newPoint % #bpHandleLeft .= pos
      else do newPoint <- newBP stem
              if segInd == 1
                then do newPoint % #bpControl .= (helP2 + pos)
                        newPoint % #bpHandleLeft .= (helP1 + pos)
                        newPoint % #bpHandleRight .= (2 *^ (helP2 + pos) - (helP1 + pos))
                else do prevPoint <- prevBP stem
                        newPoint % #bpControl .=
                          (rotate (axisAngle helAxis ((fromIntegral segInd - 1) * pi)) helP2)
                        newCo <- newPoint % #bpControl <%= (+ (bpControl prevPoint))
                        let difP = rotate
                                   (axisAngle helAxis ((fromIntegral segInd - 1) * pi))
                                   (helP2 - helP1)
                        newLeft <- newPoint % #bpHandleLeft <.= (newCo - difP)
                        newPoint % #bpHandleRight .= (2 * newCo - newLeft)
    newPoint <- getNewestBP stem
    (turtle % #turtlePos .=) =<< use (newPoint % #bpControl)
    (turtle % #turtleDir .=) =<< (use (newPoint % #bpHandleRight) <&> normalize)

    actualRadius <- calcRadiusAtOffset stem (fromIntegral segInd / fromIntegral curveRes)
    newPoint % #bpRadius .= actualRadius

    when (segInd > 0) $ do
      addSegBranchesOrLeaves tree stem turtle depth segInd curveRes branchCount
        branchNumError leafCount leafNumError prevRotAngle

      when (pointsPerSeg > 2) $
        increaseBezierPointRes stem segInd pointsPerSeg

    loop $ segInd + 1

  freeBranchCount
  freeLeafCount
  freeBNE
  freeLNE
  freePRA

  when (pointsPerSeg > 2) $
    scaleBezierHandlesForFlare stem maxPointsPerSeg

makeStemRegular :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Double -> Double -> Double 
  -> Maybe Turtle -> Maybe Turtle -> C g r ()
makeStemRegular tree stem turtle start splitCorrAngle numBranchesFactor cloneProb posCorrTurtle clonedTurtle = callCC $ \ returnEarly -> do
  whenM (use (stem % #sRadiusLimit) <&> (\ a -> 0 <= a && a < 0.001)) $
    returnEarly ()

  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  let dp1 = min (depth + 1) pMaxLevel

  when (start == 0) $ do
    setupStemLR tree stem depth dp1

  correctTurtlePos stem turtle posCorrTurtle

  case (clonedTurtle, pPruneRatio > 0) of
    (Nothing, True) -> applyPruning tree stem turtle start splitCorrAngle cloneProb returnEarly
    _               -> return ()

  let curveRes = pCurveRes V.! depth
      segSplits = pSegSplits V.! depth
  segLength <- uses (stem % #sLength) (/ fromIntegral curveRes)

  let baseSegInd = ceiling $ (pBaseSize V.! 0) * (fromIntegral $ pCurveRes V.! 0)
      nameStr = if isJust clonedTurtle then "c" else ""

  (leafCount, freeLeafCount, branchCount, freeBranchCount) <-
    leafBranchCounts tree stem start depth curveRes numBranchesFactor nameStr
            
  let maxPointsPerSeg = (ceiling :: Double -> Int) $ max 1 $ 1 / fromIntegral curveRes

  (branchNumError, freeBNE) <- newVarWithInt 0
                               ("branchNumError" ++ show depth ++ nameStr) (0 :: Double)
  (leafNumError, freeLNE) <- newVarWithInt 0
                             ("leafNumError" ++ show depth ++ nameStr) (0 :: Double)

  (prevRotAngle, freePRA) <- newPrevRotAngle depth dp1 nameStr

  let pointsPerSeg = if depth == 0 || (pTaper V.! depth) > 1
                     then maxPointsPerSeg
                     else 2

  -- MY ADDITION: CHECK IF NEEDED / CORRECT ASSERTION
  whenM (use (stem % #sCurve % #bezierPoints) <&> (not . V.null)) $
    error "Expected length of stem curve to be 0."

  (cloneProbL, freeCloneProb) <- newVarWithInt 0 ("cloneProb" ++ show depth ++ nameStr) cloneProb
  (splitCorrAngleL, freeSCA) <- newVarWithInt 0
                                ("splitCorrAngle" ++ show depth ++ nameStr) splitCorrAngle
  (numBranchesFactorL, freeNBF) <- newVarWithInt 0 ("numBranchesFactor" ++ show depth ++ nameStr)
                                   numBranchesFactor

  (loop, segInd) <- label start
  when (segInd < curveRes + 1) $ do
    let remainingSegs = curveRes + 1 - segInd
    when (segInd /= start) $
      turtle %= (move segLength)

    newPoint <- newBP stem
    pos <- use $ turtle % #turtlePos
    newPoint % #bpControl .= pos
    case (clonedTurtle, segInd == start) of
      (Just cloned, True) -> do l <- use $ stem % #sLength
                                newPoint % #bpHandleLeft .=
                                  (pos - (turtleDir cloned) ^* (l / (fromIntegral curveRes * 3)))
                                newPoint % #bpHandleRight .=
                                  (pos + (turtleDir cloned) ^* (l / (fromIntegral curveRes * 3)))
      _                   -> do l <- use $ stem % #sLength
                                dir <- use $ turtle % #turtleDir
                                newPoint % #bpHandleLeft .=
                                  (pos - dir ^* l / (fromIntegral curveRes * 3))
                                newPoint % #bpHandleRight .=
                                  (pos + dir ^* l / (fromIntegral curveRes * 3))

    actualRadius <- calcRadiusAtOffset stem (fromIntegral segInd / fromIntegral curveRes)
    newPoint % #bpRadius .= actualRadius

    when (segInd > start) $ do
      numOfSplits <- calcNumOfSplits tree depth segInd baseSegInd segSplits curveRes cloneProbL
                     numBranchesFactorL branchCount

      addSegBranchesOrLeaves tree stem turtle depth segInd curveRes branchCount branchNumError
        leafCount leafNumError prevRotAngle

      cloneStemSplits tree stem turtle depth segInd baseSegInd remainingSegs curveRes
        numOfSplits splitCorrAngleL numBranchesFactorL cloneProbL False

      applyStemTropism turtle depth

      when (pointsPerSeg > 2) $
        increaseBezierPointRes stem segInd pointsPerSeg

    loop $ segInd + 1

  freeBranchCount
  freeLeafCount
  freeBNE
  freeLNE
  freePRA
  freeCloneProb
  freeSCA
  freeNBF
  when (pointsPerSeg > 2) $
    scaleBezierHandlesForFlare stem maxPointsPerSeg

prevBP :: StemL -> C g r CurvePoint
prevBP stem = do
  l <- uses (stem % #sCurve % #bezierPoints) V.length
  when (l < 2) $
    error "There is no previous bezier point: length of curve is less than 2."
  use $ stem % #sCurve % #bezierPoints % (unsafeVectorLens $ l - 2)
    
newBP :: StemL -> C g r BPL
newBP stem = do
  stem % #sCurve % #bezierPoints %= (`V.snoc` blankBP)
  l <- uses (stem % #sCurve % #bezierPoints) V.length
  return $ stem % #sCurve % #bezierPoints % (unsafeVectorLens $ l - 1)

getNewestBP :: StemL -> C g r BPL
getNewestBP stem = do
  l <- uses (stem % #sCurve % #bezierPoints) V.length
  when (l < 1) $
    error "The curve is empty so it contains no newest bezier point."
  return $ stem % #sCurve % #bezierPoints % (unsafeVectorLens $ l - 1)

makeStem :: RandomGen g =>  TreeL -> StemL -> TurtleL -> Int -> Double -> Double -> Double
  -> Maybe Turtle -> Maybe Turtle -> C g r ()
makeStem tree stem turtle start splitCorrAngle numBranchesFactor cloneProb posCorrTurtle
  clonedTurtle = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  if pCurveRes V.! depth < 0
    then makeStemHelix tree stem turtle start splitCorrAngle numBranchesFactor cloneProb
         posCorrTurtle clonedTurtle
    else makeStemRegular tree stem turtle start splitCorrAngle numBranchesFactor cloneProb
         posCorrTurtle clonedTurtle

makeLeaves :: RandomGen g => TreeL -> StemL -> TurtleL -> Int -> Int -> DoubleL -> C g r ()
makeLeaves tree stem turtle segInd leavesOnSeg prevRotAngle =
  makeBranches tree stem turtle segInd leavesOnSeg prevRotAngle True

createBranches :: RandomGen g => TreeL -> C g r ()
createBranches tree = do
  Parameters {..} <- ask
  points <- treeBasePoints tree
  (loop, ind) <- label (0 :: Int)
  when (ind < pBranches V.! 0) $ do
    r <- getRandomR (-1, 1)
    tree % #tTreeScale .= (pGScale + r * pGScaleV)
    let (point, angle) = points V.! ind
    (turtle, freeTurtle) <- newVar "turtle" $ Turtle { turtleDir = V3 0 0 1
                                                     , turtlePos = point
                                                     , turtleRight = V3 1 0 0 }
    turtle %= (rollRight angle)
    
    stem <- newStemVar tree $ stemFromDepth 0

    makeStem tree stem turtle 0 0 1 1 Nothing Nothing

    freeTurtle

    loop (ind + 1)
    
makeTree :: RandomGen g => Bool -> C g r Tree
makeTree generateLeaves = do
  (tree, freeTree) <- newVar "tree" $
    Tree { tLeavesArray   = Nothing
         , tTreeScale     = 0
         , tBaseLength    = 0
         , tSplitNumError = V.fromList [0, 0, 0, 0, 0, 0, 0]
         , tTrunkLength   = 0
         , tStems         = V.empty }
  createBranches tree
  when generateLeaves $ undefined

  completeTree <- use tree
  freeTree
  return completeTree

_constructTree :: Parameters -> Int -> Bool -> Tree
_constructTree p g generateLeaves = evalC (makeTree generateLeaves) p (mkStdGen g)

constructTree :: Parameters -> Int -> Bool -> Tree
constructTree p g generateLeaves = 
  over #tStems
  (V.filter $ isn't (#sCurve % #bezierPoints % _Empty))
  (_constructTree p g generateLeaves)

scaleCurvePoint :: Double -> CurvePoint -> CurvePoint
scaleCurvePoint f CurvePoint {..} =
  CurvePoint 
  { bpControl = (* f) <$> bpControl
  , bpHandleLeft = (* f) <$> bpHandleLeft
  , bpHandleRight = (* f) <$> bpHandleRight
  , bpRadius = bpRadius * f
  }

scaleCurve :: Double -> Curve -> Curve
scaleCurve f Curve {..} = Curve $ fmap (scaleCurvePoint f) bezierPoints

scaleStem :: Double -> Stem -> Stem
scaleStem f stem = stem { sCurve = scaleCurve f (sCurve stem) }

scaleTree :: Double -> Tree -> Tree
scaleTree f = over #tStems (fmap (scaleStem f))

curveLargeEnough :: Double -> Curve -> Bool
curveLargeEnough minRadius Curve {..} =
  let cp = V.maximumBy
           (\ cp1 cp2 -> compare (bpRadius cp1) (bpRadius cp2))
           bezierPoints
  in bpRadius cp >= minRadius

stemLargeEnough :: Double -> Stem -> Bool
stemLargeEnough minRadius stem = curveLargeEnough minRadius (sCurve stem)

filterTree :: Double -> Tree -> Tree
filterTree minRadius = over #tStems (V.filter $ stemLargeEnough minRadius)

scaleFilterTree :: Double -> Double -> Tree -> Tree
scaleFilterTree f minRadius = (filterTree minRadius) . (scaleTree f)
