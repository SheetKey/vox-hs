{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Gox.Tree.Type where

-- gox-hs
import Gox.Turtle

-- base
import Control.Arrow

-- lens
import Control.Lens
import Control.Lens.Getter

-- mtl
import Control.Monad.State.Class
import Control.Monad.Reader.Class

-- linear
import Linear

-- vector
import qualified Data.Vector as V 

-- MonadRandom
import Control.Monad.Random.Lazy

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
  { _pShape          :: PShape    -- 0-8
  , _pGScale         :: Double    -- >0, scale of the tree
  , _pGScaleV        :: Double    -- max variation of 'gScale'
  , _pLevels         :: Int       -- > 0, often 3 or 4. Number of levels of branching
  , _pRatio          :: Double    -- > 0, ratio of the stem length to radius
  , _pRatioPower     :: Double    -- how drastically branch radius is reduced between levels
  , _pFlare          :: Double    -- how much the radius of the base trunk increases
  , _pBaseSplits     :: Int       -- number of splits at base height on trunk
  , _pBaseSize       :: V.Vector Double -- >=0, proportion of branch on which no children are spawned
  , _pDownAngle      :: V.Vector Double -- angle of child branch at level n
  , _pDownAngleV     :: V.Vector Double -- max variation of 'downAngle'. if negative then distributed along the parent stem
  , _pRotate         :: V.Vector Double -- angle about parent branch between children
  , _pRotateV        :: V.Vector Double -- variation
  , _pBranches       :: V.Vector Int    -- number of child branches at level n
  , _pLength         :: V.Vector Double -- >0, length of branches at level n as a fraction of parent branch length
  , _pLengthV        :: V.Vector Double -- variation
  , _pTaper          :: V.Vector Double -- 0-3, controls tapering of the radius of each branch
  , _pSegSplits      :: V.Vector Double -- 0-2, max number of dichotomous branches
  , _pSplitAngle     :: V.Vector Double -- angle between dichotomous branches
  , _pSplitAngleV    :: V.Vector Double -- variation
  , _pBevelRes       :: V.Vector Int    -- 
  , _pCurveRes       :: V.Vector Int    -- >0, number of segments in each branch
  , _pCurve          :: V.Vector Double -- angle that direction of stem changes about local x-axis
  , _pCurveBack      :: V.Vector Double -- angle opposite to 'curve' that the stem curves back at half way (S-shaped branches)
  , _pCurveV         :: V.Vector Double -- variation
  , _pBendV          :: V.Vector Double -- max angle that the direction of the stem may change from start to end rotating about local y-axis
  , _pBranchDist     :: V.Vector Double -- >=0, distribution of branches along parent stem
  , _pRadiusMod      :: V.Vector Double -- >=0, modifies base radius of branches
  , _pLeafBlosNum    :: Int       -- >=0, number of leaves or blossom on each of deepest level of branches
  , _pLeafShape      :: Int       -- 1-10, predefined corresponding to...
  , _pLeafScale      :: Double    -- >0, scale of leaves
  , _pLeafScaleX     :: Double    -- >0, x direction scale of leaves
  , _pLeafBend       :: Double    -- 0-1, fractional amount by which leaves are re-oriented to face the light
  , _pBlossomShape   :: Int       -- 1-3, predefined corresponding to...
  , _pBlossomScale   :: Double    -- >0, scale
  , _pBlossomRate    :: Double    -- 0-1, rate of blossoms relative to leaves
  , _pTropism        :: V3 Double -- influence growth direction in x,y, and z directions
  , _pPruneRatio     :: Double    -- 0-1, fractional amount by which pruning is applied
  , _pPruneWidth     :: Double    -- >0, width of the pruning envelope as fraction of height
  , _pPruneWidthPeak :: Double    -- >=0, fractional distance from botton of pruning up to peak
  , _pPrunePowerLow  :: Double    -- curvature of the lower section of pruning envelope
  , _pPrunePowerHigh :: Double    -- curvature of the upper section of pruning envelope
  }
makeLenses ''Parameters

data BezierPoint = BezierPoint
  { _bpControl :: V3 Double
  , _bpHandleLeft :: V3 Double
  , _bpHandleRight :: V3 Double
  , _bpRadius :: Double
  }
makeLenses ''BezierPoint

data Curve = Curve { _bezierPoints :: V.Vector BezierPoint }
makeLenses ''Curve

data Stem = Stem
  { _sDepth          :: Int
  , _sCurve          :: Curve
  , _sParent         :: Maybe Int
  , _sOffset         :: Double
  , _sRadiusLimit    :: Double
  , _sChildren       :: [Int]
  , _sLength         :: Double
  , _sRadius         :: Double
  , _sLengthChildMax :: Double
  }
makeLenses ''Stem

data Leaf = Leaf

data Tree = Tree
  { _tLeavesArray   :: Maybe (V.Vector Leaf)
  , _tStemIndex     :: Int
  , _tTreeScale     :: Double
  , _tBranchCurves  :: V.Vector Curve
  , _tBaseLength    :: Double
  , _tSplitNumError :: V.Vector Double
  , _tTrunkLength   :: Double
  , _tStems         :: V.Vector Stem
  }
makeLenses ''Tree

data TreeState = TreeState
  { _tree :: Tree
  , _stem :: Maybe Stem
  , _turtle :: Maybe Turtle
  , _depth :: Maybe Int
  , _start :: Maybe Int
  , _baseSegInd :: Maybe Int
  , _splitCorrAngle :: Maybe Double
  , _cloneProb :: Maybe Double
  , _curveRes :: Maybe Int
  , _segSplits :: Maybe Int
  , _segLength :: Maybe Double
  }
makeLenses ''TreeState

newtype T g a = T { runT :: Parameters -> g -> TreeState -> (a, TreeState, g) }

instance Functor (T g) where
  fmap f m = T $ \ p g t ->
    let ~(a, t', g') = runT m p g t
    in (f a, t', g')
  {-# INLINE fmap #-}

instance Applicative (T g) where
  pure a = T $ \ _ g t -> (a, t, g)
  {-# INLINE pure #-}

  T mf <*> T mx = T $ \ p g t ->
    let ~(f, t', g') = mf p g t
        ~(x, t'', g'') = mx p g' t'
    in (f x, t'', g'')
  {-# INLINE (<*>) #-}

instance Monad (T g) where
  m >>= k = T $ \ p g t ->
    let ~(a, t', g') = runT m p g t
        ~(b, t'', g'') = runT (k a) p g' t'
    in (b, t'', g'')
  {-# INLINE (>>=) #-}

instance MonadFail (T g) where
  fail = error 

instance RandomGen g => MonadRandom (T g) where
  getRandomR lohi = T $ \ _ g t -> let (a, g') = randomR lohi g in (a, t, g')
  getRandom = T $ \ _ g t -> let (a, g') = random g in (a, t, g')
  getRandomRs lohi = T $ \ _ g t -> let (as, g') = (first (randomRs lohi) . split) g
                                    in (as, t, g')
  getRandoms = T $ \ _ g t -> let (as, g') = (first randoms . split) g
                              in (as, t, g')

instance MonadReader Parameters (T g) where
  ask = T $ \ p g t -> (p, t, g)
  {-# INLINE ask #-}
  reader f = T $ \ p g t -> (f p, t, g)
  {-# INLINE reader #-}
  local f m = T $ runT m . f
  {-# INLINE local #-}

instance MonadState TreeState (T g) where
  get = T $ \ _ g t -> (t, t, g)
  {-# INLINE get #-}
  put t = T $ \ _ g _ -> ((), t, g)
  {-# INLINE put #-}
  state f = T $ \ _ g t -> let ~(a, t') = f t in (a, t', g)
  {-# INLINE state #-}

data TreeRG = TreeRG
  { _treeState :: TreeState
  , _parameters :: Parameters
  }
makeLenses ''TreeRG

newtype RG g a = RG { runRG :: TreeRG -> g -> (a, g) }

instance Functor (RG g) where
  fmap f m = RG $ \ trg g -> let ~(a, g') = runRG m trg g in (f a, g')
  {-# INLINE fmap #-}

instance Applicative (RG g) where
  pure a = RG $ \ _ g -> (a, g)
  {-# INLINE pure #-}

  RG mf <*> RG mx = RG $ \ trg g ->
    let ~(f, g') = mf trg g
        ~(x, g'') = mx trg g'
    in (f x, g'')
  {-# INLINE (<*>) #-}

instance Monad (RG g) where
  m >>= k = RG $ \ trg g ->
    let ~(a, g') = runRG m trg g
        ~(b, g'') = runRG (k a) trg g'
    in (b, g'')

instance MonadFail (RG g) where
  fail = error

instance MonadReader TreeRG (RG g) where
  ask = RG (,)
  {-# INLINE ask #-}
  reader f = RG $ \ trg g -> (f trg, g)
  {-# INLINE reader #-}
  local f m = RG $ \ trg g -> runRG m (f trg) g
  {-# INLINE local #-}

instance RandomGen g => MonadRandom (RG g) where
  getRandomR lohi = RG $ \ _ g -> randomR lohi g
  getRandom = RG $ \ _ g -> random g
  getRandomRs lohi = RG $ \ _ g -> (first (randomRs lohi) . split) g
  getRandoms = RG $ \ _ g -> (first randoms . split) g

data TreeR g = TreeR
  { _treeRG :: TreeRG
  , _randomGen :: g
  }
makeLenses ''TreeR

newtype R g a = R { runR :: TreeR g -> a }

instance Functor (R g) where
  fmap f m = R $ f . runR m
  {-# INLINE fmap #-}

instance Applicative (R g) where
  pure a = R $ \ _ -> a
  {-# INLINE pure #-}

  R mf <*> R mx = R $ \ r ->
    let ~f = mf r
        ~x = mx r
    in f x
  {-# INLINE (<*>) #-}

instance Monad (R g) where
  m >>= k = R $ \ r ->
    let ~a = runR m r
        ~b = runR (k a) r
    in b
  {-# INLINE (>>=) #-}

instance MonadFail (R g) where
  fail = error 

instance MonadReader (TreeR g) (R g) where
  ask = R $ \ r -> r
  {-# INLINE ask #-}
  reader f = R $ \ r -> f r
  {-# INLINE reader #-}
  local f m = R $ runR m . f
  {-# INLINE local #-}

class SubMonad m1 m2 where
  liftS :: m1 a -> m2 a

instance SubMonad (R g) (RG g) where
  liftS (R f) = RG $ \ trg g -> let a = f (TreeR trg g) in (a, g)
  {-# INLINE liftS #-}

instance SubMonad (R g) (T g) where
  liftS (R f) = T $ \ p g t -> let a = f (TreeR (TreeRG t p) g) in (a, t, g)
  {-# INLINE liftS #-}

instance SubMonad (RG g) (T g) where
  liftS (RG f) = T $ \ p g t -> let (a, g') = f (TreeRG t p) g in (a, t, g')
  {-# INLINE liftS #-}


-- pShape :: Getter Parameters PShape
-- pShape = to _pShape          
-- 
-- pGScale :: Getter Parameters Double
-- pGScale = to _pGScale
-- 
-- pGScaleV :: Getter Parameters Double
-- pGScaleV = to _pGScaleV
-- 
-- pLevels :: Getter Parameters Int
-- pLevels = to _pLevels
-- 
-- pRatio :: Getter Parameters Double
-- pRatio = to _pRatio
-- 
-- pRatioPower :: Getter Parameters Double
-- pRatioPower = to _pRatioPower
-- 
-- pFlare :: Getter Parameters Double
-- pFlare = to _pFlare
-- 
-- pBaseSplits :: Getter Parameters Int
-- pBaseSplits = to _pBaseSplits
-- 
-- pBaseSize :: Getter Parameters (V.Vector Double)
-- pBaseSize = to _pBaseSize
-- 
-- pDownAngle :: Getter Parameters (V.Vector Double)
-- pDownAngle = to _pDownAngle
-- 
-- pDownAngleV :: Getter Parameters (V.Vector Double)
-- pDownAngleV = to _pDownAngleV
-- 
-- pRotate :: Getter Parameters (V.Vector Double)
-- pRotate = to _pRotate
-- 
-- pRotateV :: Getter Parameters (V.Vector Double)
-- pRotateV = to _pRotateV
-- 
-- pBranches :: Getter Parameters (V.Vector Int)
-- pBranches = to _pBranches
-- 
-- pLength :: Getter Parameters (V.Vector Double)
-- pLength = to _pLength
-- 
-- pLengthV :: Getter Parameters (V.Vector Double)
-- pLengthV = to _pLengthV
-- 
-- pTaper :: Getter Parameters (V.Vector Double)
-- pTaper = to _pTaper
-- 
-- pSegSplits :: Getter Parameters (V.Vector Double)
-- pSegSplits = to _pSegSplits
-- 
-- pSplitAngle :: Getter Parameters (V.Vector Double)
-- pSplitAngle = to _pSplitAngle
-- 
-- pSplitAngleV :: Getter Parameters (V.Vector Double)
-- pSplitAngleV = to _pSplitAngleV
-- 
-- pBevelRes :: Getter Parameters (V.Vector Int)
-- pBevelRes = to _pBevelRes
-- 
-- pCurveRes :: Getter Parameters (V.Vector Int)
-- pCurveRes = to _pCurveRes
-- 
-- pCurve :: Getter Parameters (V.Vector Double)
-- pCurve = to _pCurve
-- 
-- pCurveBack :: Getter Parameters (V.Vector Double)
-- pCurveBack = to _pCurveBack
-- 
-- pCurveV :: Getter Parameters (V.Vector Double)
-- pCurveV = to _pCurveV
-- 
-- pBendV :: Getter Parameters (V.Vector Double)
-- pBendV = to _pBendV
-- 
-- pBranchDist :: Getter Parameters (V.Vector Double)
-- pBranchDist = to _pBranchDist
-- 
-- pRadiusMod :: Getter Parameters (V.Vector Double)
-- pRadiusMod = to _pRadiusMod
-- 
-- pLeafBlosNum :: Getter Parameters Int
-- pLeafBlosNum = to _pLeafBlosNum
-- 
-- pLeafShape :: Getter Parameters Int
-- pLeafShape = to _pLeafShape
-- 
-- pLeafScale :: Getter Parameters Double
-- pLeafScale = to _pLeafScale
-- 
-- pLeafScaleX :: Getter Parameters Double
-- pLeafScaleX = to _pLeafScaleX
-- 
-- pLeafBend :: Getter Parameters Double
-- pLeafBend = to _pLeafBend
-- 
-- pBlossomShape :: Getter Parameters Int
-- pBlossomShape = to _pBlossomShape
-- 
-- pBlossomScale :: Getter Parameters Double
-- pBlossomScale = to _pBlossomScale
-- 
-- pBlossomRate :: Getter Parameters Double
-- pBlossomRate = to _pBlossomRate
-- 
-- pTropism :: Getter Parameters (V3 Double)
-- pTropism = to _pTropism
-- 
-- pPruneRatio :: Getter Parameters Double
-- pPruneRatio = to _pPruneRatio
-- 
-- pPruneWidth :: Getter Parameters Double
-- pPruneWidth = to _pPruneWidth
-- 
-- pPruneWidthPeak :: Getter Parameters Double
-- pPruneWidthPeak = to _pPruneWidthPeak
-- 
-- pPrunePowerLow :: Getter Parameters Double
-- pPrunePowerLow = to _pPrunePowerLow
-- 
-- pPrunePowerHigh :: Getter Parameters Double
-- pPrunePowerHigh = to _pPrunePowerHigh
