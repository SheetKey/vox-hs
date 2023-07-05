{-# LANGUAGE RecordWildCards #-}

module Gox.Tree where

-- gox-hs
import Gox.Turtle

-- base
import Control.Arrow
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- MonadRandom
import Control.Monad.Random.Lazy

-- lens
import Control.Lens.Getter ((^.))

validateParameters :: Parameters -> Bool
validateParameters = undefined

data BezierPoint = BezierPoint
  { bpControl :: V3 Double
  , bpHandleLeft :: V3 Double
  , bpHandleRight :: V3 Double
  , bpRadius :: Double
  }

data Curve = Curve { bezierPoints :: V.Vector BezierPoint }

data Stem = Stem
  { sDepth          :: Int
  , sCurve          :: Curve
  , sParent         :: Maybe Int
  , sOffset         :: Double
  , sRadiusLimit    :: Double
  , sChildren       :: [Int]
  , sLength         :: Double
  , sRadius         :: Double
  , sLengthChildMax :: Double
  }

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

data Leaf = Leaf

data Tree = Tree
  { tLeavesArray   :: Maybe (V.Vector Leaf)
  , tStemIndex     :: Int
  , tTreeScale     :: Double
  , tBranchCurves  :: V.Vector Curve
  , tBaseLength    :: Double
  , tSplitNumError :: V.Vector Double
  --, tTreeObj       :: Maybe TreeObj
  , tTrunkLength   :: Double
  , tStems         :: V.Vector Stem
  }

data PShape = Spherical
            | Hemispherical
            | Cylindrical
            | TaperedCylindrical
            | Flame
            | InverseConical
            | TendFlame
            | Envelope
            | Conical

data Parameters = Parameters
  { pShape          :: PShape    -- 0-8
  , pGScale         :: Double    -- >0, scale of the tree
  , pGScaleV        :: Double    -- max variation of 'gScale'
  , pLevels         :: Int       -- > 0, often 3 or 4. Number of levels of branching
  , pRatio          :: Double    -- > 0, ratio of the stem length to radius
  , pRatioPower     :: Double    -- how drastically branch radius is reduced between levels
  , pFlare          :: Double    -- how much the radius of the base trunk increases
  , pBaseSplits     :: Int       -- number of splits at base height on trunk
  , pBaseSize       :: V.Vector Double -- >=0, proportion of branch on which no children are spawned
  , pDownAngle      :: V.Vector Double -- angle of child branch at level n
  , pDownAngleV     :: V.Vector Double -- max variation of 'downAngle'. if negative then distributed along the parent stem
  , pRotate         :: V.Vector Double -- angle about parent branch between children
  , pRotateV        :: V.Vector Double -- variation
  , pBranches       :: V.Vector Int    -- number of child branches at level n
  , pLength         :: V.Vector Double -- >0, length of branches at level n as a fraction of parent branch length
  , pLengthV        :: V.Vector Double -- variation
  , pTaper          :: V.Vector Double -- 0-3, controls tapering of the radius of each branch
  , pSegSplits      :: V.Vector Double -- 0-2, max number of dichotomous branches
  , pSplitAngle     :: V.Vector Double -- angle between dichotomous branches
  , pSplitAngleV    :: V.Vector Double -- variation
  , pBevelRes       :: V.Vector Int    -- 
  , pCurveRes       :: V.Vector Int    -- >0, number of segments in each branch
  , pCurve          :: V.Vector Double -- angle that direction of stem changes about local x-axis
  , pCurveBack      :: V.Vector Double -- angle opposite to 'curve' that the stem curves back at half way (S-shaped branches)
  , pCurveV         :: V.Vector Double -- variation
  , pBendV          :: V.Vector Double -- max angle that the direction of the stem may change from start to end rotating about local y-axis
  , pBranchDist     :: V.Vector Double -- >=0, distribution of branches along parent stem
  , pRadiusMod      :: V.Vector Double -- >=0, modifies base radius of branches
  , pLeafBlosNum    :: Int       -- >=0, number of leaves or blossom on each of deepest level of branches
  , pLeafShape      :: Int       -- 1-10, predefined corresponding to...
  , pLeafScale      :: Double    -- >0, scale of leaves
  , pLeafScaleX     :: Double    -- >0, x direction scale of leaves
  , pLeafBend       :: Double    -- 0-1, fractional amount by which leaves are re-oriented to face the light
  , pBlossomShape   :: Int       -- 1-3, predefined corresponding to...
  , pBlossomScale   :: Double    -- >0, scale
  , pBlossomRate    :: Double    -- 0-1, rate of blossoms relative to leaves
  , pTropism        :: V3 Double -- influence growth direction in x,y, and z directions
  , pPruneRatio     :: Double    -- 0-1, fractional amount by which pruning is applied
  , pPruneWidth     :: Double    -- >0, width of the pruning envelope as fraction of height
  , pPruneWidthPeak :: Double    -- >=0, fractional distance from botton of pruning up to peak
  , pPrunePowerLow  :: Double    -- curvature of the lower section of pruning envelope
  , pPrunePowerHigh :: Double    -- curvature of the upper section of pruning envelope
  }

newtype TreeState g a = TreeState
  { runTreeState :: Parameters -> g -> Tree -> Stem -> (a, Stem, Tree, g) }

instance Functor (TreeState g) where
  fmap f m = TreeState $ \ p g t s ->
    let ~(a, s', t', g') = runTreeState m p g t s
    in (f a, s', t', g')
  {-# INLINE fmap #-}

instance Applicative (TreeState g) where
  pure a = TreeState $ \ _ g t s -> (a, s, t, g)
  {-# INLINE pure #-}
  TreeState mf <*> TreeState mx = TreeState $ \ p g t s ->
    let ~(f, s', t', g') = mf p g t s
        ~(x, s'', t'', g'') = mx p g' t' s'
    in (f x, s'', t'', g'')
  {-# INLINE (<*>) #-}

instance Monad (TreeState g) where
  m >>= k = TreeState $ \ p g t s ->
    let ~(a, s', t', g') = runTreeState m p g t s
        ~(b, s'', t'', g'') = runTreeState (k a) p g' t' s'
    in (b, s'', t'', g'')
  {-# INLINE (>>=) #-}

instance RandomGen g => MonadRandom (TreeState g) where
  getRandomR lohi = TreeState $ \ _ g t s -> let (a, g') = randomR lohi g in (a, s, t, g')
  getRandom = TreeState $ \ _ g t s -> let (a, g') = random g in (a, s, t, g')
  getRandomRs lohi = TreeState $ \ _ g t s ->
                                   let (as, g') = (first (randomRs lohi) . split) g
                                   in (as, s, t, g')
  getRandoms = TreeState $ \ _ g t s ->
                             let (as, g') = (first randoms . split) g
                             in (as, s, t, g')

reader :: (Parameters -> a) -> TreeState g a
reader = asks
{-# INLINE reader #-}

ask :: TreeState g Parameters
ask = TreeState $ \ p g t s -> (p, s, t, g)
{-# INLINE ask #-}

asks :: (Parameters -> a) -> TreeState g a
asks f = TreeState $ \ p g t s -> (f p, s, t, g)
{-# INLINE asks #-}

tState :: (Tree -> (a, Tree)) -> TreeState g a
tState f = TreeState $ \ _ g t s -> let (a, t') = f t in (a, s, t', g)
{-# INLINE tState #-}

sState :: (Stem -> (a, Stem)) -> TreeState g a
sState f = TreeState $ \ _ g t s -> let (a, s') = f s in (a, s', t, g)
{-# INLINE sState #-}
  
tGet :: TreeState g Tree
tGet = TreeState $ \ _ g t s -> (t, s, t, g)
{-# INLINE tGet #-}

sGet :: TreeState g Stem
sGet = TreeState $ \ _ g t s -> (s, s, t, g)
{-# INLINE sGet #-}

gGet :: TreeState g g
gGet = TreeState $ \ _ g t s -> (g, s, t, g)
{-# INLINE gGet #-}

tPut :: Tree -> TreeState g ()
tPut t = TreeState $ \ _ g _ s -> ((), s, t, g)
{-# INLINE tPut #-}

sPut :: Stem -> TreeState g ()
sPut s = TreeState $ \ _ g t _ -> ((), s, t, g)
{-# INLINE sPut #-}

gPut :: g -> TreeState g ()
gPut g = TreeState $ \ _ _ t s -> ((), s, t, g)
{-# INLINE gPut #-}

tModify :: (Tree -> Tree) -> TreeState g ()
tModify f = TreeState $ \ _ g t s -> ((), s, f t, g)
{-# INLINE tModify #-}

sModify :: (Stem -> Stem) -> TreeState g ()
sModify f = TreeState $ \ _ g t s -> ((), f s, t, g)
{-# INLINE sModify #-}

tGets :: (Tree -> a) -> TreeState g a
tGets f = TreeState $ \ _ g t s -> (f t, s, t, g)
{-# INLINE tGets #-}

sGets :: (Stem -> a) -> TreeState g a
sGets f = TreeState $ \ _ g t s -> (f s, s, t, g)
{-# INLINE sGets #-}

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
toTrackQuatZY v =
  let axis = if abs (v ^._x) + abs (v ^._y) < (10 ^ (-4))
            then V3 1 (v ^._x) 0
            else V3 (negate (v ^._y)) (v ^._x) 0
      co = (v ^._z) / 3
      q = axisAngle axis (saacos co)
      mat = fromQuaternion q
      fp = mat ^._z
      angle = -0.5 * atan2 (negate fp ^._x) (negate fp ^._y)
      co' = cos angle
      si' = (sin angle) / 3
      q2 = Quaternion co' (v ^* si')
  in mulQtQt q q2

calcHelixPoints :: RandomGen g => Turtle -> Double -> Double
  -> TreeState g (V3 Double, V3 Double, V3 Double, V3 Double)
calcHelixPoints turtle rad pitch = do
  spinAng <- getRandomR (0, 2 * pi)
  let p0 = V3 0 (negate rad) (negate pitch / 4)
      p1 = V3 (4 * rad/ 3) (negate rad) 0
      p2 = V3 (4 * rad / 3) rad 0
      p3 = V3 0 rad (pitch / 4) 
      trf = toTrackQuatZY (turtleDir turtle)
      rotQuat = Quaternion spinAng (V3 0 0 1)
      p0' = rotate trf $ rotate rotQuat p0
      p1' = rotate trf $ rotate rotQuat p1
      p2' = rotate trf $ rotate rotQuat p2
      p3' = rotate trf $ rotate rotQuat p3
  return (p1' - p0', p2' - p0', p3' - p0', turtleDir turtle)

calcShapeRatio :: PShape -> Double -> TreeState g Double
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

calcStemLength :: RandomGen g => TreeState g Double
calcStemLength = do
  Tree {..} <- tGet
  stem <- sGet 
  let depth = sDepth stem
  case depth of
    -- trunk
    0 -> do Parameters {..} <- ask
            randomUniform <- getRandomR (-1, 1)
            let length = tTreeScale * ((pLength V.! 0) + (randomUniform * (pLengthV V.! 0)))
            tModify $ \ tree -> tree { tTrunkLength = length }
            return $ max 0 length
    -- first level
    1 -> do let Just parentIdx = sParent stem
                parent = tStems V.! parentIdx
            Parameters {..} <- ask
            offset <- sGets sOffset
            shapeRatio <- calcShapeRatio pShape $
                          (sLength parent - offset) / (sLength parent - tBaseLength)
            let length = sLength parent * sLengthChildMax parent * shapeRatio
            return $ max 0 length
    _ -> do let Just parentIdx = sParent stem
                parent = tStems V.! parentIdx
            offset <- sGets sOffset
            let length = (sLengthChildMax parent) * (sLength parent - 0.7 * offset)
            return $ max 0 length
            
calcStemRadius :: TreeState g Double
calcStemRadius = do
  Parameters {..} <- ask
  depth <- sGets sDepth
  stem <- sGet
  if depth == 0
    then return $ sLength stem * pRatio * (pRadiusMod V.! 0) 
    else do Tree {..} <- tGet
            let Just parentIdx = sParent stem
                parent = tStems V.! parentIdx
                result = pRadiusMod V.! (sDepth stem) * sRadius parent *
                         ((sLength stem / sLength parent) ** pRatioPower)
                result' = max 0.005 result
                result'' = min (sRadiusLimit stem) result'
            return result''

calcCurveAngle :: RandomGen g => Int -> Int -> TreeState g Double
calcCurveAngle depth segInd = do
  Parameters {..} <- ask
  let curve = pCurve V.! depth
      curveBack = pCurveBack V.! depth
      curveRes = pCurveRes V.! depth
      dCurveRes = fromIntegral curveRes
      curveV = pCurveV V.! depth
  curveAngle <- if curveBack == 0
                then return $ curve / dCurveRes
                else if fromIntegral segInd < dCurveRes / 2
                     then return $ curve / (dCurveRes / 2)
                     else return $ curveBack / (dCurveRes / 2)
  randomUniform <- getRandomR (-1, 1)
  return $ curveAngle + randomUniform * (curveV / dCurveRes)

calcDownAngel :: RandomGen g => Double -> TreeState g Double
calcDownAngel stemOffset = do
  Parameters {..} <- ask
  stem <- sGet
  let depth = sDepth stem
      depthPlus = min (depth + 1) pLevels
  if pDownAngleV V.! depthPlus >= 0
    then do randomUniform <- getRandomR (-1, 1)
            return $ pDownAngle V.! depthPlus + randomUniform * pDownAngleV V.! depthPlus
    else do ratio <- calcShapeRatio Spherical $
                     (sLength stem - stemOffset) / (sLength stem * (1 - pBaseSize V.! depth))
            let dAngle = pDownAngle V.! depthPlus + pDownAngleV V.! depthPlus *
                         (1 - 2 * ratio)
            randomUniform <- getRandomR (-1, 1)
            return $ dAngle + randomUniform * abs (dAngle * 0.1)

calcRotateAngle :: RandomGen g => Int -> Double -> TreeState g Double
calcRotateAngle depth prevAngle = do
  Parameters {..} <- ask
  let rotate = pRotate V.! depth
      rotateV = pRotateV V.! depth
  if rotate >= 0
    then do randomUniform <- getRandomR (-1, 1)
            return $ mod' (prevAngle + rotate + randomUniform * rotateV) 360
    else do randomUniform <- getRandomR (-1, 1)
            return $ prevAngle * (180 + rotate + randomUniform * rotateV)

calcLeafCount :: TreeState g Double
calcLeafCount = do
  Parameters {..} <- ask
  stem <- sGet
  if pLeafBlosNum >= 0
    then do Tree {..} <- tGet
            let leaves = fromIntegral pLeafBlosNum * tTreeScale / pGScale
                Just parentIdx = sParent stem
                parent = tStems V.! parentIdx
            return $ leaves *
              (sLength stem / (sLengthChildMax parent * sLength parent))
    else return $ fromIntegral pLeafBlosNum 

calcBranchCount :: RandomGen g => TreeState g Double
calcBranchCount = do
  Parameters {..} <- ask
  stem <- sGet
  stems <- tGets tStems
  let depth = sDepth stem
      depthPlus = min (depth + 1) pLevels
      branches = fromIntegral $ pBranches V.! depthPlus
  result <- if depth == 0
            then do randomUniform <- getRandomR (0, 1)
                    return $ branches * (randomUniform * 0.2 + 0.9)
            else if branches < 0
                 then return $ branches
                 else let Just parentIdx = sParent stem
                          parent = stems V.! parentIdx
                      in if depth == 1
                      then return $ branches * (0.2 + 0.8 * 
                                                 (sLength stem / sLength parent) /
                                                 (sLengthChildMax parent))
                      else return $ branches *
                           (1 - 0.5 * sOffset stem / sLength parent)
  return $ result / (1 - (pBaseSize V.! depth))

calcRadiusAtOffset :: Double -> TreeState g Double
calcRadiusAtOffset z1 = do
  Parameters {..} <- ask
  stem <- sGet
  let nTaper = pTaper V.! sDepth stem
      unitTaper = case (nTaper < 1, nTaper < 2) of
                    (True, _) -> nTaper
                    (_, True) -> 2 - nTaper
                    _         -> 0
      taper = sRadius stem * (1 - unitTaper * z1)
      radius = if nTaper < 1
               then taper
               else let z2 = (1 - z1) * sLength stem
                        depth = if nTaper < 2 || z2 < taper
                                then 1
                                else nTaper - 2
                        z3 = if nTaper < 2
                             then z2
                             else abs $ z2 - 2 * taper *
                                  (fromIntegral . truncate) (z2 / (2 * taper) + 0.5)
                    in if nTaper < 2 && z3 >= taper
                       then taper
                       else (1 - depth) * taper + depth * sqrt ((taper ^ 2) - (z3 - taper) ^ 2)
  if sDepth stem == 0
    then let yVal = max 0 (1 - 9 * z1)
             flare = pFlare * (100 ** yVal) / 100 + 1
         in return $ radius * flare
    else return radius

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

-- callsight assures that pointsPerSeg > 2
increaseBezierPointRes :: Int -> Int -> TreeState g ()
increaseBezierPointRes segInd pointsPerSeg = do
  Parameters {..} <- ask
  depth <- sGets sDepth
  curve <- sGets $ bezierPoints . sCurve
  let curveRes = fromIntegral $ pCurveRes V.! depth
      curveNumPoints = V.length curve
      segEndPoint = curve V.! (curveNumPoints - 1)
      segStartPoint = curve V.! (curveNumPoints - 2)
      dSegInd = fromIntegral segInd
      dPointsPerSeg = fromIntegral pointsPerSeg
  -- update last two entries in the curve
  r1 <- calcRadiusAtOffset $ (dSegInd - 1) / curveRes
  let pntCo = calcPointOnBezier (1 / (dPointsPerSeg - 1)) segStartPoint segEndPoint
      tangent = normalize $ calcTangentToBezier (1 / (dPointsPerSeg - 1)) segStartPoint segEndPoint
      dirVecMag = norm $ bpHandleLeft segEndPoint - bpHandleRight segStartPoint
      pntHL = pntCo - tangent ^* dirVecMag
      pntHR = pntCo + tangent ^* dirVecMag
  r2 <- calcRadiusAtOffset $
    ((1 / (dPointsPerSeg - 1)) + (dSegInd) - 1) / curveRes
  sModify $ \ Stem {..} -> Stem
    { sCurve = Curve $ bezierPoints sCurve V.//
               [ (curveNumPoints - 2, segStartPoint { bpRadius = r1 })
               , (curveNumPoints - 1, BezierPoint pntCo pntHL pntHR r2)
               ], .. }
  -- calc vector to be concatenated
  newPnts <- V.generateM (pointsPerSeg - 2) $ \i -> do
        let k = i + 2
            offset = (fromIntegral k) / (dPointsPerSeg - 1)
            (co, hl, hr) = if k == pointsPerSeg - 1
                           then ( bpControl segEndPoint
                                , bpHandleLeft segEndPoint
                                , bpHandleRight segEndPoint)
                           else let co = calcPointOnBezier offset segStartPoint segEndPoint
                                    tangent = normalize $
                                              calcTangentToBezier offset segStartPoint segEndPoint
                                    hl = co - tangent ^* dirVecMag
                                    hr = co + tangent ^* dirVecMag
                                in (co, hl, hr)
        radius <- calcRadiusAtOffset $ (offset + (dSegInd) - 1) / curveRes
        return $ BezierPoint co hl hr radius
  sModify $ \ Stem {..} -> Stem { sCurve = Curve $ bezierPoints sCurve V.++ newPnts, .. }

pointInside :: V3 Double -> TreeState g Bool
pointInside point = do
  Parameters {..} <- ask
  Tree {..} <- tGet
  let dist = sqrt $ ((point ^._x) ^ 2) + ((point ^._y) ^ 2)
      ratio = (tTreeScale - (point ^._z)) / (tTreeScale * (1 - (pBaseSize V.! 0)))
  shapeRatio <- calcShapeRatio Envelope ratio
  return $ (dist / tTreeScale) < (pPruneWidth * shapeRatio)

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

applyTropism :: Turtle -> V3 Double -> Turtle
applyTropism turtle tropismVector =
  let hcrosst = turtleDir turtle `cross` tropismVector
      alpha = pi * (10 * norm hcrosst) / 180
      nhcrosst = normalize hcrosst
      dir = normalize $ rotate (axisAngle nhcrosst alpha) (turtleDir turtle)
      right = normalize $ rotate (axisAngle nhcrosst alpha) (turtleRight turtle)
  in turtle { turtleDir = dir, turtleRight = right }

scaleBezierHandlesForFlare :: Int -> TreeState g ()
scaleBezierHandlesForFlare maxPointsPerSeg = do
  sModify $ \ stem -> let curve = sCurve stem
                          curve' = Curve $
                            (\BezierPoint {..} -> BezierPoint
                              { bpHandleLeft = bpControl
                                + (bpHandleLeft - bpControl) ^/ fromIntegral maxPointsPerSeg
                              , bpHandleRight = bpControl
                                + (bpHandleRight - bpControl) ^/ fromIntegral maxPointsPerSeg
                              , ..
                              } ) <$> bezierPoints curve 
                      in stem { sCurve = curve' }

trunkBaseSplit :: RandomGen g => TreeState g (V.Vector (V3 Double, Double))
trunkBaseSplit = do
  Parameters {..} <- ask
  tModify $ \ Tree {..} -> Tree { tTreeScale = pGScale + pGScaleV, .. }
  sPut (stemFromDepth 0)
  length <- calcStemLength
  sModify $ \ stem -> stem { sLength = length }
  radius <- calcStemRadius 
  genPointsAngles radius ((pBranches V.! 0) - 1) V.empty
  where
    newPointAngle = do
      Parameters {..} <- ask
      rand <- getRandomR (0, 1)
      let dis = sqrt $ rand * (fromIntegral $ pBranches V.! 0) / 2.5 * pGScale * pRatio
      theta <- getRandomR (0, 2 * pi)
      return $ (V3 (dis * cos theta) (dis * sin theta) 0, theta)
    checkNewPoint rad (pos, theta) = V.all (\(p, _) -> norm (pos - p) < rad) 
    genPointsAngles rad i acc | i <= 0 = return acc
                              | otherwise = do
                                  pa <- newPointAngle
                                  if checkNewPoint rad pa acc
                                    then genPointsAngles rad (i - 1) (acc `V.snoc` pa)
                                    else genPointsAngles rad i acc

makeTree :: RandomGen g => TreeState g Tree
makeTree = do
  Parameters {..} <- ask
  case pBranches V.! 0 of
    1 -> do
      angle <- getRandomR (0, 2 * pi)
      sPut (stemFromDepth 0)
      makeStem (rollRight angle turtle) 0 0 1 1 Nothing Nothing
    i -> do
      pas <- trunkBaseSplit
      makeTreeBranches (i - 1) pas
  tGet
  where
    turtle = Turtle
             { turtleDir = V3 0 0 1
             , turtlePos = V3 0 0 0
             , turtleRight = V3 1 0 0 }
    makeTreeBranches i pas | i == 0 = let nTurtle = (rollRight
                                                      (snd (pas V.! i) - (pi / 2))
                                                      turtle
                                                    )
                                                    { turtlePos = fst (pas V.! i) }
                                      in do sPut (stemFromDepth 0)
                                            makeStem nTurtle 0 0 1 1 Nothing Nothing
                           | otherwise = do let nTurtle = (rollRight
                                                            (snd (pas V.! i) - (pi / 2))
                                                            turtle
                                                          )
                                                          { turtlePos = fst (pas V.! i) }
                                            sPut (stemFromDepth 0)
                                            makeStem nTurtle 0 0 1 1 Nothing Nothing
                                            makeTreeBranches (i - 1) pas

makeStem :: RandomGen g => Turtle -> Int -> Double -> Double -> Double -> Maybe Turtle
  -> Maybe Turtle -> TreeState g ()
makeStem turtle start splitCorrAngle numBranchesFactor cloneProb mposCorrTurtle mclonedTurtle
  = do
  radiusLimit <- sGets sRadiusLimit
  if 0 <= radiusLimit && radiusLimit < 0.0001
    then return ()
    else do
    Parameters {..} <- ask
    depth <- sGets sDepth
    let depthPlus = if depth + 1 > pLevels then pLevels else depth + 1
    when (start == 0) $ do
      rand <- getRandomR (-1, 1)
      sModify $ \ stem -> stem
        { sLengthChildMax =
            (pLength V.! depthPlus) + rand * (pLengthV V.! depthPlus)
        }
      length <- calcStemLength
      sModify $ \ stem -> stem { sLength = length }
      radius <- calcStemRadius
      sModify $ \ stem -> stem { sRadius = radius }
      when (depth == 0) $ tModify $ \ tree -> tree { tBaseLength = length * (pBaseSize V.! 0) }
    r <- sGets sRadius
    rl <- sGets sRadiusLimit
    let turtle' = case mposCorrTurtle of
          Just posCorrTurtle -> let posCorrTurtle' = move (negate $ min r rl) posCorrTurtle
                                in turtle { turtlePos = turtlePos posCorrTurtle' }
          Nothing -> turtle
    case (mclonedTurtle, pPruneRatio > 0) of
      (Nothing, True) -> do
        startLength <- sGets sLength
        rState <- gGet
        splitErrState <- tGets tSplitNumError
        inPruningEnvelope <- testStem turtle' start splitCorrAngle cloneProb
        return ()
      _ -> return ()

testStem :: RandomGen g => Turtle -> Int -> Double -> Double -> TreeState g (Bool, Turtle)
testStem turtle start splitCorrAngle cloneProb = do
  Parameters {..} <- ask
  stem <- sGet
  let depth = sDepth stem
      depthPlus = if depth + 1 > pLevels then pLevels else depth + 1
      curveRes = pCurveRes V.! depth
      segSplits = pSegSplits V.! depth
      segLength = sLength stem / fromIntegral curveRes
      baseSegInd = ceiling $ (pBaseSize V.! 0) * (fromIntegral $ pCurveRes V.! 0)
  if pCurveV V.! depth < 0
    then testStemHelix turtle start splitCorrAngle cloneProb depth
         curveRes segSplits segLength baseSegInd
    else testStemRegular turtle start splitCorrAngle cloneProb depth
         curveRes segSplits segLength baseSegInd

testStemHelix :: RandomGen g => Turtle -> Int -> Double -> Double -> Int -> Int -> Double
  -> Double -> Int -> TreeState g (Bool, Turtle)
testStemHelix turtle start splitCorrAngle cloneProb depth curveRes segSplits
  segLength baseSegInd = do
  r1 <- getRandomR (0.8, 1.2)
  r2 <- getRandomR (0.8, 1.2)
  curveV <- asks pCurveV
  length <- sGets sLength
  let tanAng = tan $ (pi / 2) - abs (curveV V.! depth)
      helPitch = 2 * length / (fromIntegral curveRes) * r1
      helRadius = 3 * helPitch / (16 * tanAng) * r2
  turtle' <- applyFullTropism turtle depth
  (_, _, hel2, helAxis) <- calcHelixPoints turtle' helRadius helPitch
  (_, turtle'', _, _) <- iterateUntilM
    (\(i, _, _, _) -> i == curveRes + 1)
    (start, turtle', hel2, V3 0 0 0) $
    \ (segInd, turt, helP2, prevHel) -> do
      let pos = turtlePos turt
          (pos', helP2') = if segInd == 0
                           then (pos, helP2)
                           else if segInd == 1
                                then (hel2 + pos, helP2)
                                else let q = Quaternion ((fromIntegral segInd - 1) * pi) helAxis
                                         p = rotate q helP2
                                     in (prevHel + p, p)
          prevHel' = pos'
      return (segInd + 1, turt { turtlePos = pos' }, helP2', prevHel')
  tf <- pointInside (turtlePos turtle'')
  return (tf, turtle'')

testStemRegular :: RandomGen g => Turtle -> Int -> Double -> Double -> Int -> Int -> Double
  -> Double -> Int -> TreeState g (Bool, Turtle)
testStemRegular turtle start splitCorrAngle cloneProb depth curveRes segSplits segLength
  baseSegInd = do
  (_, turtle', success, _, _) <- iterateUntilM
    (\ (i, _, tf, _, _) -> i == curveRes + 1 || not tf)
    (start, turtle, True, cloneProb, splitCorrAngle)
    (testStemRegularIteration start depth curveRes baseSegInd segSplits segLength)
  if success
    then do isInside <- pointInside (turtlePos turtle')
            return (isInside, turtle')
    else return (False, turtle')

testStemRegularIteration :: RandomGen g => Int -> Int -> Int -> Int -> Double -> Double
  -> (Int, Turtle, Bool, Double, Double) -> TreeState g (Int, Turtle, Bool, Double, Double)
testStemRegularIteration start depth curveRes baseSegInd segSplits segLength 
  (segInd, turtle, _, cloneProb, splitCorrAngle) = do
  let remainingSegs = curveRes + 1 - segInd
  (turtle', hasNewPoint) <- setUpNextNormalBP turtle depth start segInd baseSegInd segLength
  if hasNewPoint
    then if segInd > start
         then
           do (numOfSplits, cloneProb') <-
                calcNumOfSplits depth segInd baseSegInd curveRes segSplits cloneProb
              if numOfSplits > 0
                then do (sprAngle, splAngle, splitCorrAngle', isBaseSplit, usingDirectSplit) <-
                          calcAnglesForSplit turtle' depth remainingSegs segInd baseSegInd
                        let turtle'' = applySplitBaseStem turtle' splAngle sprAngle
                                       isBaseSplit usingDirectSplit numOfSplits
                        turtle''' <- applyFullTropism turtle'' depth
                        return (segInd + 1, turtle''', True, cloneProb', splitCorrAngle')
                else do turtle'' <-
                          applyCurveSplitCorr turtle' depth curveRes segInd splitCorrAngle
                        turtle''' <- applyFullTropism turtle'' depth
                        return (segInd + 1, turtle''', True, cloneProb', splitCorrAngle)
         else return (segInd + 1, turtle', True, cloneProb, splitCorrAngle)
    else return (segInd + 1, turtle', False, cloneProb, splitCorrAngle)

applyFullTropism :: Turtle -> Int -> TreeState g Turtle
applyFullTropism turtle depth = do
  tropism <- asks pTropism
  if depth > 1
    then return $ applyTropism turtle tropism
    else return $ applyTropism turtle (V3 (tropism ^._x) (tropism ^._y) 0)

setUpNextNormalBP :: Turtle -> Int -> Int -> Int -> Int -> Double -> TreeState g (Turtle, Bool)
setUpNextNormalBP turtle depth start segInd baseSegInd segLength = 
  if segInd /= start
  then let turtle' = move segLength turtle
       in
         do isInside <- pointInside (turtlePos turtle')
            if not (depth == 0 && start < baseSegInd) && not isInside
              then return (turtle', False)
              else return (turtle', True)
    else return (turtle, True)

calcNumOfSplits :: RandomGen g => Int -> Int -> Int -> Int -> Double -> Double
  -> TreeState g (Int, Double)
calcNumOfSplits depth segInd baseSegInd curveRes segSplits cloneProb = do
  Parameters {..} <- ask
  if pBaseSplits > 0 && depth == 0 && segInd == baseSegInd
    then
    do r <- getRandomR (0 :: Double, 1)
       return (truncate $ r * (fromIntegral pBaseSplits) + 0.5, cloneProb)
    else if segSplits > 0 && segInd < curveRes && (depth > 0 || segInd > baseSegInd)
         then 
           do r <- getRandomR (0, 1)
              if r <= cloneProb
                then
                do splitNumError <- tGets tSplitNumError
                   let numOfSplits = truncate $ segSplits + (splitNumError V.! depth)
                       cloneProb' = cloneProb / (fromIntegral numOfSplits + 1)
                   tModify $ \ tree -> tree
                     { tSplitNumError = tSplitNumError tree V.//
                       [( depth
                        , (tSplitNumError tree V.! depth)
                          - ((fromIntegral numOfSplits) - segSplits))]
                     }
                   return (numOfSplits, cloneProb')
                else return (0, cloneProb)
         else return (0, cloneProb)

calcAnglesForSplit :: RandomGen g => Turtle -> Int -> Int -> Int -> Int
  -> TreeState g (Double, Double, Double, Bool, Bool)
calcAnglesForSplit turtle depth remainingSegs segInd baseSegInd = do
  Parameters {..} <- ask
  let isBaseSplit = pBaseSplits > 0 && depth == 0 && segInd == baseSegInd
      usingDirectSplit = pSplitAngle V.! depth < 0
  if usingDirectSplit
    then
    do
      r <- getRandomR (-1, 1)
      let sprAngle = (abs (pSplitAngle V.! depth)) + r * (pSplitAngleV V.! depth)
          splAngle = 0
          splitCorrAngle = 0
      return (sprAngle, splAngle, splitCorrAngle, isBaseSplit, usingDirectSplit)
    else
    do
      r1 <- getRandomR (-1, 1)
      r2 <- getRandomR (0, 1)
      let dec = declination $ turtleDir turtle
          splAngle = max 0 $ (pSplitAngle V.! depth) + r1 * (pSplitAngleV V.! depth) - dec
          splitCorrAngle = splAngle / (fromIntegral remainingSegs)
          sprAngle = negate $ 20 + 0.75 * (30 + (abs $ dec - (pi / 2)) * r2 * r2)
      return (sprAngle, splAngle, splitCorrAngle, isBaseSplit, usingDirectSplit)
          
applySplitBaseStem :: Turtle -> Double -> Double -> Bool -> Bool -> Int -> Turtle
applySplitBaseStem turtle splAngle sprAngle isBaseSplit usingDirectSplit numOfSplits =
  let turtle' = pitchDown (splAngle / 2) turtle
  in if not isBaseSplit && numOfSplits == 1
     then if usingDirectSplit
          then turnLeft (sprAngle / 2) turtle'
          else let dir' = normalize $ rotate
                          (Quaternion (negate $ sprAngle / 2) (V3 0 0 1))
                          (turtleDir turtle')
                   right' = normalize $ rotate
                            (Quaternion (negate $ sprAngle / 2) (V3 0 0 1))
                            (turtleRight turtle')
               in turtle' { turtleDir = dir', turtleRight = right' }
     else turtle'

applyCurveSplitCorr :: RandomGen g => Turtle -> Int -> Int -> Int -> Double -> TreeState g Turtle
applyCurveSplitCorr turtle depth curveRes segInd splitCorrAngle = do
  Parameters {..} <- ask
  r <- getRandomR (-1, 1)
  let t' = turnLeft (r * (pBendV V.! depth) / (fromIntegral curveRes)) turtle
  curveAngle <- calcCurveAngle depth segInd
  return $ pitchDown (curveAngle - splitCorrAngle) t'
        
declination :: V3 Double -> Double
declination (V3 x y z) = atan2 (sqrt $ x * x + y * y) z

iterateUntilM :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
iterateUntilM p v f = go p f v
  where
    go p f v
      | p v       = return v
      | otherwise = f v >>= go p f

whenV :: Monad m => Bool -> a -> m a -> m a
whenV p a m
  | p         = m
  | otherwise = return a
