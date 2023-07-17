{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Gox.Tree.Algorithm where

-- gox-hs
import Gox.Turtle
import Gox.Tree.Type

-- base
import Data.Maybe (fromJust)
import Data.Fixed (mod')

-- linear
import Linear

-- lens
import qualified Control.Lens.Getter as L
import qualified Control.Lens.Setter as L

-- optics-core
import Optics.Core

-- optics-extra
import Optics.State
import Optics.State.Operators
import Optics.At

-- mtl
import Control.Monad.State.Class
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
  , sChildren       = []
  , sLength         = 0
  , sRadius         = 0
  , sLengthChildMax = 0
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
  let axis = if abs x + abs y < (10 ^ (-4))
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

calcPointOnBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcPointOnBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in (invOffset ^ 3) *^ co1
          + 3 * (invOffset ^ 2) * offset *^ hr1
          + 3 * invOffset * (offset ^ 2) *^ hl2
          + (offset ^ 3) *^ co2

calcTangentToBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcTangentToBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in 2 * (invOffset ^ 2) *^ (hr1 - co1)
          + 6 * invOffset * offset *^ (hl2 - hr1)
          + 3 * (offset ^ 2) *^ (co2 - hl2)

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
  result <- callCC $ \ break -> do
    whenM (use (stem % #sDepth) <&> (== 0)) $ do
      r <- getRandomR (-1, 1)
      treeScale <- use $ tree % #tTreeScale
      res <- tree % #tTrunkLength <.= (treeScale * ((pLength V.! 0) + r * (pLengthV V.! 0)))
      break res
    whenM (use (stem % #sDepth) <&> (== 1)) $ do
      parent <- getParent tree stem
      baseLength <- use $ tree % #tBaseLength
      offset <- use $ stem % #sOffset
      shapeRatio <- calcShapeRatio pShape $
        (parent^. #sLength - offset) / (parent^. #sLength - baseLength)
      break $ (parent^. #sLength) * (parent^. #sLengthChildMax) * shapeRatio
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
  let dp1 = min (depth + 1) pLevels
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
    let dp1 = min (depth + 1) pLevels
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
                                        (fromIntegral . truncate) (z2 / (2 * taper) + 0.5)
    when (nTaper < 2 && z3 >= taper) $
      break taper
    return $ (1 - depth) * taper + depth * sqrt ((taper ^ 2) - ((z3 - taper) ^ 2))
  d <- use $ stem % #sDepth
  if d == 0
    then let yVal = max 0 (1 - 8 * z1)
             flare = pFlare * (100 ** yVal) / 100 + 1
         in  return $ radius * flare
    else return radius

blankBP :: BezierPoint
blankBP = BezierPoint (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) 0

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
      stem % #sCurve % #bezierPoints % (ix $ curveNumPoints - 2 + k) .= (BezierPoint co hl hr 0)
    -- set radius of new bp point
    radiusAtOffset <- calcRadiusAtOffset stem $ (offset + fromIntegral segInd - 1) / curveRes
    stem % #sCurve % #bezierPoints % (ix $ curveNumPoints - 2 + k) % #bpRadius .= radiusAtOffset
    -- loop
    loop (k + 1)

pointInside :: TreeL -> V3 Double -> C g r Bool
pointInside tree (V3 x y z) = do
  Parameters {..} <- ask
  treeScale <- use $ tree % #tTreeScale
  let dist = sqrt $ (x ^ 2) + (y ^ 2)
      ratio = (treeScale - z) / (treeScale * (1 - (pBaseSize V.! 0)))
  shapeRatio <- calcShapeRatio Envelope ratio
  return $ (dist / treeScale) < (pPruneWidth * shapeRatio)

makeBranchPosTurtle :: Turtle -> Double -> BezierPoint -> BezierPoint -> Double -> (Turtle, Turtle)
makeBranchPosTurtle dirTurtle offset startPoint endPoint radiusLimit =
  let pos = calcPointOnBezier offset startPoint endPoint
      dirTurtle' = dirTurtle { turtlePos = pos }
      branchPosTurtle = move radiusLimit $ pitchDown (pi / 2) dirTurtle'
  in (dirTurtle', branchPosTurtle)

makeBranchDirTurtle :: Turtle -> Bool -> Double -> BezierPoint -> BezierPoint -> Turtle
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
    (\ BezierPoint {..} -> BezierPoint
      { bpHandleLeft = bpControl
        + (bpHandleLeft - bpControl) ^/ fromIntegral maxPointsPerSeg
      , bpHandleRight = bpControl
        + (bpHandleRight - bpControl) ^/ fromIntegral maxPointsPerSeg })

pointsForFloorSplit :: RandomGen g => TreeL -> C g r (V.Vector (V3 Double, Double))
pointsForFloorSplit tree = do
  Parameters {..} <- ask
  tree % #tTreeScale .= pGScale + pGScaleV
  let dummyStem = stemFromDepth 0
      branches = pBranches V.! 0
  (dummyStemL, freeDummyStem) <- newVar "dummyStem" dummyStem
  l <- calcStemLength tree dummyStemL 
  rad <- calcStemRadius tree dummyStemL
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
  let dp1 = min (depth + 1) pLevels
      curveRes = pCurveRes V.! depth
      segSplits = pSegSplits V.! depth
      baseSegInd = ceiling $ (pBaseSize V.! 0) * (fromIntegral $ pCurveRes V.! 0)
  segLength <- use (stem % #sLength) <&> (/ fromIntegral curveRes) 
  (_, _, helP2, helAxis) <- calcHelixParameters stem turtle
  (loop, (segInd, prevHelPnt)) <- label (start, V3 0 0 0) 
  when (segInd < curveRes + 1) $ do
    let remainingSegs = curveRes + 1 - segInd
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
  let dp1 = min (depth + 1) pLevels
      curveRes = pCurveRes V.! depth
      segSplits = pSegSplits V.! depth
      baseSegInd = ceiling $ (pBaseSize V.! 0) * (fromIntegral $ pCurveRes V.! 0)
  segLength <- use (stem % #sLength) <&> (/ fromIntegral curveRes)
  callCC $ \ break -> do
    (cpL, freecp) <- newVar "cp" cloneProb
    (numOfSplits, freeNumOfSplits) <- newVar "numOfSplits" (0 :: Int)
    (splitCorrAngleL, freeSCA) <- newVar "splitCorrAngle" splitCorrAngle
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
        if nos > 0
          then do let isBaseSplit = pBaseSplits > 0 && depth == 0 && segInd == baseSegInd
                      usingDirectSplit = pSplitAngle V.! depth < 0
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
                              ((pi / 9) + 0.75 * ((pi / 6) + abs(dec - (pi / 2)) * (r2 ^ 2)))
                  spl <- use splAngle
                  turtle %= (pitchDown (spl / 2))
                  when (not isBaseSplit && nos == 1) $ 
                    if usingDirectSplit
                    then do spr <- use sprAngle
                            turtle %= (turnLeft (spr / 2))
                    else do spr <- use sprAngle
                            turtle % #turtleDir %=
                              (normalize . (rotate (axisAngle (V3 0 0 1) (negate spr / 2))))
                            turtle % #turtleRight %=
                              (normalize . (rotate (axisAngle (V3 0 0 1) (negate spr / 2))))
                  freesprAngle
                  freesplAngle
          else do r <- getRandomR (-1, 1)
                  turtle %= turnLeft (r * (pBendV V.! depth) / fromIntegral curveRes)
                  curveAngle <- calcCurveAngle depth segInd
                  sca <- use splitCorrAngleL
                  turtle %= pitchDown (curveAngle - sca)
        if depth > 1
          then turtle %= applyTropism pTropism
          else turtle %= applyTropism (pTropism & (L..~)_z 0)
      loop (segInd + 1)
    freecp
    freeNumOfSplits
    freeSCA
    tPos <- use $ turtle % #turtlePos
    pointInside tree tPos
        
testStem :: RandomGen g => TreeL -> StemL -> TurtleL -> C g r Bool
testStem tree stem turtle = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  if pCurveV V.! depth < 0
    then testStemHelix tree stem turtle 0
    else testStemRegular tree stem turtle 0 0 1

setUpBranch :: RandomGen g => StemL -> TurtleL -> DoubleL -> BranchMode -> Double -> BezierPoint
  -> BezierPoint -> Double -> Int -> Int -> C g r (Turtle, Turtle, Double, Double)
setUpBranch stem turtle prevRotAngle branchMode offset startPoint endPoint stemOffset branchInd branchesInGroup
  = do
  Parameters {..} <- ask
  depth <- use $ stem % #sDepth
  turt <- use turtle
  let dp1 = min pLevels (depth + 1)
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

--makeBranches :: RandomGen g => Int -> Int -> Bool -> MS g r ()
--makeBranches segInd branchesOnSeg isLeaves = do
--  Parameters {..} <- ask
--  curve <- use #sCurve
--  let curveNumPoints = V.length curve
--      segEndPoint = curve V.! (curveNumPoints - 1)
--      segStartPoint = curve V.! (curveNumPoints - 2)
--      dp1 = min (depth + 1) pLevels
--  if branchesOnSeg < 0 -- fan branches
--    then do (loop, branchInd) <- label (0 :: Int)
--            when (branchInd < abs branchesOnSeg) $ do
--              
--              loop (branchInd + 1)
