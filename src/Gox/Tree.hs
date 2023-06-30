module Gox.Tree where

-- gox-hs
import Gox.Turtle

validateParameters :: Parameters -> Bool

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
  , sParent         :: Int
  , sOffset         :: Double
  , sRadiusLimit    :: Double
  , sChildren       :: [Int]
  , sLength         :: Double
  , sRadius         :: Double
  , sLengthChildMax :: Int
  }

stemFromDepth :: Int -> Stem
stemFromDepth sDepth = Stem
  { sCurve          = Nothing
  , sParent         = Nothing
  , sOffset         = 0
  , sRadiusLimit    = -1
  , sChildren       = []
  , sLength         = 0
  , sRadius         = 0
  , sLengthChildMax = 0
  , ..
  }

data Tree = Tree
  { tLeavesArray   :: Maybe (Vector Leaf)
  , tStemIndex     :: Int
  , tTreeScale     :: Int
  , tBranchCurves  :: Vector Curve
  , tBaseLength    :: Double
  , tSplitNumError :: [Int]
  , tTreeObj       :: Maybe TreeObj
  , tTrunkLength   :: Double
  , tStems         :: Vector Stem
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
  , pDownAngleV     :: V.Vector Int    -- max variation of 'downAngle'. if negative then distributed along the parent stem
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
  , pCurveRes       :: V.Vector Double -- >0, number of segments in each branch
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
        ~(b, s'', t'', g'') <- runTreeState (k a) p g' t' s'
    in (b, s'', t'', g'')
  {-# INLINE (>>=) #-}

instance RandomGen g => MonadRandom (TreeState g) where
  getRandomR lohi = TreeState $ \ _ g t s -> let (a, g') = randomR lohi g in (a, s, t, g')
  getRandom = TreeState $ \ _ g t s -> let (a, g') = random g in (a, s, t, g')
  getRandomRs lohi = TreeState $ \ _ g t s ->
                                   let (as, g') = first (randomRs lohi) . split g
                                   in (a, s, t, g')
  getRandoms = TreeState $ \ _ g t s ->
                             let (as, g') = first randoms . split g
                             in (a, s, t, g')

reader :: (Parameters -> a) -> TreeState g a
reader = asks
{-# INLINE reader #-}

ask :: TreeState g Parameters
ask = TreeState $ \ p g t s -> (p, s t, g)
{-# INLINE ask #-}

asks :: (Parameters -> a) -> TreeState g a
asks f = TreeState $ \ p g t s -> (f r, s, t, g)
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
sModify f = TreeState $ \ _ g t s -> ((), f s, t g)
{-# INLINE sModify #-}

tGets :: (Tree -> a) -> TreeState g a
tGets f = TreeState $ \ _ g t s -> (f t, s, t, g)
{-# INLINE tGets #-}

sGets :: (Shape -> a) -> TreeState g a
sGets f = TreeState $ \ _ g t s -> (f s, s, t, g)
{-# INLINE sGets #-}

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
                   then return $ (ratio / (1 - pPruneWidthPeak)) ^ pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - pPruneWidthPeak)) ^ pPrunePowerLow
    Conical -> return $ 0.2 + 0.8 * ratio

calcStemLength :: TreeState g Double
calcStemLength = do
  Tree {..} <- tGet
  depth <- sGets sDepth
  case depth of
    -- trunk
    0 -> do Parameters {..} <- ask
            randomUniform <- getRandomR (-1, 1)
            let length = tTreeScale * ((length V.! 0) + (randomUniform * (pLengthV V.! 0)))
            tModify $ \ tree -> tree { tTrunkLength = length }
            return $ max 0 length
    -- first level
    1 -> do parentIdx <- sGets sParent
            let parent = tStems V.! parentIdx
            Parameters {..} <- ask
            offset <- sGets sOffset
            shapeRatio <- calcShapeRatio pShape $
                          (sLength parent - offset) / (sLength parent - tBaseLength)
            let length = sLength parent * sLengthChildMax parent * shapeRatio
            return $ max 0 length
    _ -> do parentIdx <- sGets sParent
            let parent = tStems V.! parentIdx
            offset <- sGets sOffset
            let length = sLengthChildMax parent * (sLength parent - 0.7 * offset)
            return $ max 0 length
            
calcStemRadius :: TreeState g Double
calcStemRadius = do
  Parameters {..} <- ask
  depth <- sGets sDepth
  if depth == 0
    then return $ sLength stem * pRatio * (pRadiusMod V.! 0) 
    else do Tree {..} <- tGet
            parentIdx <- sGets sParent
            let parent = tStems V.! sParent stem
                result = pRadiusMod V.! (sDepth stem) * sRadius parent *
                         ((sLength stem / sLength parent) ^ pRatioPower)
                result' = max 0.005 result
                result'' = min (sRadiusLimit stem) result'
            return result''

calcCurveAngle :: Double -> Double -> TreeState g Double
calcCurveAngle depth segInd = do
  Parameters {..} <- ask
  let curve = pCurve V.! depth
      curveBack = pCurveBack V.! depth
      curveRes = pCurveRes V.! depth
      curveV = pCurveV V.! depth
  curveAngle <- if curveBack == 0
                then return $ curve / curveRes
                else if segInd < curveRes / 2
                     then return $ curve / (curveRes / 2)
                     else return $ curveBack / (curveRes / 2)
  randomUniform <- getRandomR (-1, 1)
  return $ curveAngle + randomUniform * (curveV / curveRes)

calcDownAngel :: Double -> TreeState g Double
calcDownAngel stemOffset = do
  Parameters {..} <- ask
  stem <- sGet
  let depthPlus = min (sDepth stem + 1) pMaxDepth
  if pDownAngleV V.! depthPlus >= 0
    then do randomUniform <- getRandomR (-1, 1)
            return $ downAngle V.! depthPlus + randomUniform * downAngleV V.! depthPlus
    else do let dAngle = downAngle V.! depthPlus + downAngleV V.! depthPlus *
                         (1 - 2 * (calcShapeRatio 0 $ (length stem - stemOffset) /
                                   (length stem * (1 - baseSize V.! depth))))
            randomUniform <- getRandomR (-1, 1)
            return $ dAngle + randomUniform * abs (dAngle * 0.1)

calcRotateAngle :: Double -> Double -> TreeState g Double
calcRotateAngle depth prevAngle = do
  Parameters {..} <- ask
  let rotate = pRotate V.! depth
      rotateV = pRotateV V.! depth
  if rotate >= 0
    then do randomUniform <- getRandomR (-1, 1)
            return $ mod (prevAngle + rotate + randomUniform * rotateV) 360
    else do randomUniform <- getRandomR (-1, 1)
            return $ prevAngle * (180 + rotate + randomUniform * rotateV)

calcLeafCount :: TreeState g Int
calcLeafCount = do
  Parameters {..} <- ask
  stem <- sGet
  if pLeafBlosNum >= 0
    then do Tree {..} <- get
            let leaves = pLeafBlosNum * tTreeScale / pGScale
                parent = tStems V.! sParent stem 
            return $ leaves * (sLength stem / (sLengthChildMax parent * sLength parent))
    else return $ sLeafBlosNum stem

calcBranchCount :: TreeState g Int
calcBranchCount = do
  Parameters {..} <- ask
  stem <- sGet
  let depth = sDepth stem
      depthPlus = min (depth + 1) pMaxDepth
      branches = pBranches V.! depthPlus
  result <- if depth == 0
            then do randomUniform <- getRandomR (0, 1)
                    return $ branches * (randomUniform * 0.2 + 0.9)
            else if branches < 0
                 then return $ branches
                 else let parent = tStems V.! sParent stem
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
                             else abs $ z2 - 2 * taper * truncate (z2 / (2 * taper) + 0.5)
                    in if nTaper < 2 && z3 >= taper
                       then taper
                       else (1 - depth) * taper + depth * sqrt ((taper ^ 2) - (z3 - taper) ^ 2)
  if sDepth stem == 0
    then let yVal = max 0 (1 - 9 * z1)
             flare = pFlare * (100 ^ yVal) / 100 + 1
         in return $ radius * flare
    else return radius

calcPointOnBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcPointOnBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           1mOffset = 1 - offset 
       in (1mOffset ^ 3) *^ co1
          + 3 * (1mOffset ^ 2) * offset *^ hr1
          + 3 * 1mOffset * (offset ^ 2) *^ hl2
          + (offset ^ 3) *^ co2

calcTangentToBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcTangentToBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           1mOffset = 1 - offset 
       in 2 * (1mOffset ^ 2) *^ (hr1 - co1)
          + 6 * 1mOffset * offset *^ (hl2 - hr1)
          + 3 * (offset ^ 2) *^ (co2 - hl2)

-- callsight assures that pointsPerSeg > 2
increaseBezierPointRes :: Double -> Double -> TreeState g ()
increaseBezierPointRes segInd pointsPerSeg = do
  Parameters {..} <- ask
  depth <- sGets sDepth
  curve <- sGets sCurve
  let curveRes = truncate $ pCurveRes V.! depth
      curveNumPoints = V.length curve
      segEndPoint = curve V.! curveNumPoints - 1
      segStartPoint = curve V.! curveNumPoints - 2
  -- update last two entries in the curve
  r1 <- calcRadiusAtOffset $ (segInd - 1) / curveRes
  let pntCo = calcPointOnBezier (1 / (pointsPerSeg - 1)) segStartPoint segEndPoint
      tangent = normalize $ calcTangentToBezier (1 / (pointsPerSeg - 1)) segStartPoint segEndPoint
      dirVecMag = norm $ bpHandleLeft segEndPoint - bpHandleRight segStartPoint
      pntHL = pntCo - tangent ^* dirVecMag
      pntHR = pntCo + tangent ^* dirVecMag
  r2 <- calcRadiusAtOffset $ ((1 / (pointsPerSeg - 1)) + segInd - 1) / curveRes
  sModify $ \ Stem {..} -> Stem
    { sCurve = sCurve V.// [ (curveNumPoints - 2, segStartPoint { bpRadius = r1 })
                           , (curveNumPoints - 1, BezierPoint pntCo pntHL pntHR r2)
                           ], .. }
  -- calc vector to be concatenated
  newPnts <- V.generateM (pointsPerSeg - 2) $ \i -> do
        let k = i + 2
            offset = k / (pointsPerSeg - 1)
            (co, hl, hr) = if k = pointsPerSeg - 1
                           then ( bpControl segEndPoint
                                , bpHandleLeft segEndpoint
                                , bpHandleRight segEndPoint)
                           else let co = calcPointOnBezier offset segStartPoint segEndPoint
                                    tangent = normalize $
                                              calcTangentToBezier offset segStartPoint segEndPoint
                                    dirVecMag = norm 
                                                (bpHandleLeft segEndPoint
                                                 - bpHandleRight segStartPoint)
                                    hl = co - tangent ^* dirVecMag
                                    hr = co + tangent ^* dirVecMag
                                in (co, hl, hr)
        radius <- calcRadiusAtOffset $ (offset + segInd - 1) / curveRes
        return $ BezierPoint co hl hr radius
  sModify $ \ Stem {..} -> Stem { sCurve = sCurve V.++ newPnts }

trunkBaseSplit :: TreeState g (V.Vector (V3 Double, Double))
trunkBaseSplit = do
  Parameters {..} <- ask
  modify $ \ Tree {..} -> Tree { tTreeScale = pGScale + pGScaleV, .. }
  stem <- calcStemRadius =<< calcStemLength (stemFromDepth 0)
  let radius = 2.5 * sRadius stem
  genPointsAngles radius ((pBranches V.! 0) - 1) V.empty
  where
    newPointAngle = do
      Parameters {..} <- ask
      rand <- getRandomR (0, 1)
      let dis = sqrt $ rand * (pBranches V.! 0) / 2.5 * pGScale * pRatio
      theta <- getRandomR (0, 2 * pi)
      return $ (V3 (dis * cos theta) (dis * sin theta) 0, theta)
    checkNewPoint rad (pos, theta) = V.all (\(p, _) -> norm pos p < rad)
    genPointsAngles rad i acc | i <= 0 = acc
                              | otherwise = do
                                  pa <- newPointAngle
                                  if checkNewPoint rad pa
                                    then genPointsAngles rad (i - 1) (acc V.snoc pa)
                                    else genPointsAngles rad i acc

makeTree :: TreeState g Tree
makeTree = do
  Parameters {..} <- ask
  case pBranches V.! 0 of
    1 -> do
      angle <- getRandomR (0, 2 * pi)
      makeStemBasic (rollRight angle turtle) (stemFromDepth 0)
    i -> do
      pas <- trunkBaseSplit
      makeTreeBranches (i - 1) pas
  get
  where
    turtle = Turtle
             { turtleDir = V3 0 0 1
             , turtlePos = V3 0 0 0
             , turtleRight = V3 1 0 0 }
    makeTreeBranches i pas | i == 0 = let nTurtle = (rollRight (snd (pas V.! i) - (pi / 2)))
                                                    { turtlePos = fst (pas V.! i) }
                                      in makeStemBasic nTurtle (stemFromDepth 0)
                           | otherwise = do let nTurtle = (rollRight (snd (pas V.! i) - (pi / 2)))
                                                          { turtlePos = fst (pas V.! i) }
                                            makeStemBasic nTurtle (stemFromDepth 0)
                                            makeTreeBranches (i - 1) pas

makeStemBasic :: Turtle -> Stem -> TreeState g ()
makeStemBasic turtle stem = makeStemMaybe turtle stem Nothing Nothing Nothing Nothing Nothing

makeStemMaybe :: Turtle -> Stem -> Maybe Int -> Maybe Double -> Maybe Int
  -> Maybe Turtle -> Maybe Turtle -> TreeState g ()
makeStemMaybe turtle stem mStart mSplitCorrAngle mNumBranchesFactor mCloneProb =
  makeStem turtle stem start splitCorrAngle numBranchesFactor cloneProb
  where
    start = fromMaybe 0 mStart
    splitCorrAngle = fromMaybe 0 mSplitCorrAngle
    numBranchesFactor = fromMaybe 1 mNumBranchesFactor
    cloneProb = fromMaybe 1 mCloneProb

makeStem :: Turtle -> Stem -> Int -> Double -> Int -> Double
  -> Maybe Turtle -> Maybe Turtle -> TreeState g ()
makeStem turtle stem start splitCorrAngle
  numBranchesFactor cloneProb mPosCorrTurtle mClonedTurtle = do 
  if abs sRadiusLimit < 0.0001 -- check if abs is neede? does negative sRadiusLimit have meaning?
    then return ()
    else do
    Parameters {..} <- ask
    let depth = sDepth stem
        depthPlus = if depth + 1 >= pLevels -- check this: Why does og not use levels here?
                    then depth
                    else depth + 1
    -- calc stem length if not a clone
    stem' <- if start == 0
             then do rand <- getRandomR (-1, 1)
                     let lengthChildMax = (pLength V.! depthPlus) + rand * (pLengthV V.! depthPlus)
                     s <- calcStemRadius =<< calcStemLength
                          (stem { sLengthChildMax = lengthChildMax })
                     if depth == 0
                       then do modify $ \ tree -> tree
                                                  { tBaseLength = sLength s * (pBaseSize V.! 0) }
                               return s
                       else return s
               else return stem
    -- reposition turtle if necessary
    turtle' <- case mPosCorrTurtle of
                 Nothing -> return turtle
                 Just posCorrTurtle ->
                   let pos = turtlePos $ move
                             (- min (sRadius stem') (sRadiusLimit stem'))
                             posCorrTurtle
                   in return $ turtle { turtlePos = pos }
    -- apply pruning (only for non-clones)
    mStem'' <- case (mClonedTurtle, pPruneRatio > 0) of
      (Nothing, True) -> do
        let startLength = sLength stem'
            splitErrState = tSplitNumError
        rState <- getG
        mTempStem <- applyPruning splitErrState rState startLength pPruneRatio turtle' stem'
          start splitCorrAngle cloneProb
        case mTempStem of
          Nothing -> return $ Nothing
          Just tempStem -> do
            let fittingLength = sLength tempStem
                length = startLength * (1 - pPruneRatio) + fittingLength * pPruneRatio
            newStem <- calcStemRadius (tempStem { sLength = length })
            putG rState
            modify $ \ tree -> tree { tSplitNumError = splitErrState }
            return $ Just $ Just newStem
      _ -> return $ Just stem'
    case mStem'' of
      Nothing -> return () -- mStem'' is too small so delete entirely
      Just stem'' -> do
        -- get parameters
        let curveRes = truncate $ pCurveRes V.! depth
            segSplits = pSegSplits V.! depth
            segLength = sLength stem''
        -- calc base segment
            baseSegInd = ceiling $ (pBaseSize V.! 0) * truncate (pCurveRes V.! 0)
            (leafCount, branchCount) = 
                   
applyPruning :: [Int] -> g -> Double -> Double -> Turtle -> Stem -> Int
  -> Double -> Double -> TreeState g (Maybe Stem)
applyPruning splitErrState rState startLength pruneRatio turtle stem
  start splitCorrAngle cloneProb = do
  inPruningEnvelope <- testStem turtle stem start splitCorrAngle cloneProb
  if not inPruningEnvelope
    then let length = sLength stem * 0.9
         in if length < 0.15 * startLength 
            then if pruneRatio < 1
                 then return . Just $ stem { sLength = 0 }
                 else return Nothing
            else do putG rState
                    modify $ \ tree -> tree { tSplitNumError = splitErrState }
                    applyPruning splitErrState rState startLength prunRatio turtle
                      (stem { sLength = length }) start splitCorrAngle cloneProb
    else return . Just $ stem
