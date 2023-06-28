module Gox.Tree where

-- gox-hs
import Gox.Turtle

validateParameters :: Parameters -> Bool

data BezierPoint = BezierPoint
  { control :: V3 Double
  , handleLeft :: V3 Double
  , handleRight :: V3 Double
  }

data Curve = Curve { bezierPoints :: V.Vector BezierPoint }

data Stem = Stem
  { sDepth          :: Int
  , sCurve          :: Maybe Curve
  , sParent         :: Maybe Int
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

data Parameters = Parameters
  { pShape          :: Int       -- 0-8
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

newtype TreeState g a = TreeState { runTreeState :: Parameters -> g -> Tree -> (a, Tree, g) }

instance Functor (TreeState g) where
  fmap f m = TreeState $ \ p g t ->
    let ~(a, t', g') = runTreeState m p g t
    in (f a, t', g')
  {-# INLINE fmap #-}

instance Applicative (TreeState g) where
  pure a = TreeState $ \ _ g t -> (a, t, g)
  {-# INLINE pure #-}
  TreeState mf <*> TreeState mx = TreeState $ \ p g t ->
    let ~(f, t', g') = mf p g t
        ~(x, t'', g'') = mx p g' t'
    in (f x, t'', g'')
  {-# INLINE (<*>) #-}

instance Monad (TreeState g) where
  m >>= k = TreeState $ \ p g t ->
    let ~(a, t', g') = runTreeState m p g t
        ~(b, t'', g'') <- runTreeState (k a) p g' t'
    in (b, t'', g'')
  {-# INLINE (>>=) #-}

instance RandomGen g => MonadRandom (TreeState g) where
  getRandomR lohi = TreeState $ \ _ g t -> let (a, g') = randomR lohi g in (a, t, g')
  getRandom = TreeState $ \ _ g t -> let (a, g') = random g in (a, t, g')
  getRandomRs lohi = TreeState $ \ _ g t ->
                                   let (as, g') = first (randomRs lohi) . split g
                                   in (a, t, g')
  getRandoms = TreeState $ \ _ g t ->
                             let (as, g') = first randoms . split g
                             in (a, t, g')

reader :: (Parameters -> a) -> TreeState g a
reader = asks
{-# INLINE reader #-}

ask :: TreeState g Parameters
ask = TreeState $ \ p g t -> (p, t, g)
{-# INLINE ask #-}

asks :: (Parameters -> a) -> TreeState g a
asks f = TreeState $ \ p g t -> (f r, t, g)
{-# INLINE asks #-}

state :: (Tree -> (a, Tree)) -> TreeState g a
state f = TreeState $ \ _ g t -> let (a, t') = f t in (a, t', g)
{-# INLINE state #-}
  
get :: TreeState g Tree
get = TreeState $ \ _ g t -> (t, t, g)
{-# INLINE get #-}

put :: Tree -> TreeState g ()
put t = TreeState $ \ _ g _ -> ((), t, g)
{-# INLINE put #-}

modify :: (Tree -> Tree) -> TreeState g ()
modify f = TreeState $ \ _ g t -> ((), f t, g)
{-# INLINE modify #-}

gets :: (Tree -> a) -> TreeState g a
gets f = TreeState $ \ _ g t -> (f t, t, g)
{-# INLINE gets #-}

getG :: TreeState g g
getG = TreeState $ \ _ g t -> (g, t, g)
{-# INLINE getG #-}

putG :: g -> TreeState g ()
putG g = TreeState $ \ _ _ t -> ((), t, g)
{-# INLINE putG #-}

calcShapeRatio :: Int -> Double -> TreeState g Double
calcShapeRatio shape ratio =
  case shape of
    -- spherical
    1 -> return $ 0.2 + 0.8 * sin (pi * ratio)
    -- hemispherical
    2 -> return $ 0.2 + 0.8 * sin (0.5 * pi * ratio)
    -- cylindrical
    3 -> return 1
    -- tapered cylindrical
    4 -> return $ 0.5 + 0.5 * ratio
    -- flame
    5 -> return $ if ratio <= 0.7 then ratio / 0.7 else (1 - ratio) / 0.3
    -- inverse conical
    6 -> return $ 1 - 0.8 * ratio
    -- tend flame
    7 -> return $ if ratio <= 0.7 then 0.5 + 0.5 * ratio / 0.7 else 0.5 + 0.5 * (1 - ratio) / 0.3
    -- envelope
    8 -> if ratio < 0 || ratio > 1
         then return 0
         else do Parameters {..} <- ask
                 if ratio < 1 - pPruneWidthPeak
                   then return $ (ratio / (1 - pPruneWidthPeak)) ^ pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - pPruneWidthPeak)) ^ pPrunePowerLow
    -- conical
    _ -> return $ 0.2 + 0.8 * ratio

calcStemLength :: Stem -> TreeState g Stem
calcStemLength stem = do
  Tree {..} <- get
  case sDepth stem of
    -- trunk
    0 -> do Parameters {..} <- ask
            randomUniform <- getRandomR (-1, 1)
            let result = tTreeScale * (length V.! 0 + (randomUniform * pLengthV V.! 0))
            modify $ \ Tree {..} -> Tree { tTrunkLength = result, .. }
            return $ stem { sLength = max 0 result }
    -- first level
    1 -> do let parent = tStems V.! sParent stem
            Parameters {..} <- ask
            shapeRatio <- calcShapeRatio pShape $
                          (sLength parent - sOffset stem) / (sLength parent - tBaseLength)
            return $ stem { sLength = sLength parent * sLengthChildMax * shapeRatio }
    _ -> let parent = tStems V.! sParent stem
         in return $ stem
            { sLength = sLengthChildMax parent * (sLength parent - 0.7 * sOffset stem) }
            
calcStemRadius :: Stem -> TreeState g Stem
calcStemRadius stem = do
  Parameters {..} <- ask
  if sDepth stem == 0
    then return $ stem { sRadius = sLength stem * pRatio * (pRadiusMod V.! 0) }
    else do Tree {..} <- get
            let parent = tStems V.! sParent stem
                result = pRadiusMod V.! (sDepth stem) * sRadius parent *
                         ((sLength stem / sLength parent) ^ pRatioPower)
                result' = max 0.005 result
                result'' = min (sRadiusLimit stem) result'
            return $ stem { sRadius = result'' }

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

calcDownAngel :: Stem -> Double -> TreeState g Double
calcDownAngel stem stemOffset = do
  Parameters {..} <- ask
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

calcLeafCount :: Stem -> TreeState g Int
calcLeafCount stem = do
  Parameters {..} <- ask
  if pLeafBlosNum >= 0
    then do Tree {..} <- get
            let leaves = pLeafBlosNum * tTreeScale / pGScale
                parent = tStems V.! sParent stem 
            return $ leaves * (sLength stem / (sLengthChildMax parent * sLength parent))
    else return sLeafBlosNum

calcBranchCount :: Stem -> TreeState g Int
calcBranchCount stem = do
  Parameters {..} <- ask
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

calcRadiusAtOffset :: Stem -> Int -> TreeState g Double
calcRadiusAtOffset stem z1 = do
  Parameters {..} <- ask
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
