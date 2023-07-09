{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Gox.Tree.Type where

-- gox-hs
import Gox.Turtle

-- base
import Control.Arrow
import GHC.Generics (Generic)

-- optics-core
import Optics.Optic
import Optics.Lens

-- mtl
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class

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

data BezierPoint = BezierPoint
  { bpControl :: V3 Double
  , bpHandleLeft :: V3 Double
  , bpHandleRight :: V3 Double
  , bpRadius :: Double
  }
  deriving (Show, Generic)

data Curve = Curve { bezierPoints :: V.Vector BezierPoint }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

data Leaf = Leaf
  deriving Show

data Tree = Tree
  { tLeavesArray   :: Maybe (V.Vector Leaf)
  , tStemIndex     :: Int
  , tTreeScale     :: Double
  , tBranchCurves  :: V.Vector Curve
  , tBaseLength    :: Double
  , tSplitNumError :: V.Vector Double
  , tTrunkLength   :: Double
  , tStems         :: V.Vector Stem
  }
  deriving (Show, Generic)

data MakeStem = MakeStem
  { tLeavesArray    :: Maybe (V.Vector Leaf)
  , tStemIndex      :: Int
  , tTreeScale      :: Double
  , tBranchCurves   :: V.Vector Curve
  , tBaseLength     :: Double
  , tSplitNumError  :: V.Vector Double
  , tTrunkLength    :: Double
  , tStems          :: V.Vector Stem
  , sDepth          :: Int
  , sCurve          :: Curve
  , sParent         :: Maybe Int
  , sOffset         :: Double
  , sRadiusLimit    :: Double
  , sChildren       :: [Int]
  , sLength         :: Double
  , sRadius         :: Double
  , sLengthChildMax :: Double
  , turtle          :: Turtle
  , start           :: Int
  , splitCorrAngle  :: Double
  , numBranchesFactor :: Double
  , cloneProb :: Double
  , possCorrTurtle :: Maybe Turtle
  , clonedTurtle :: Maybe Turtle
  , numOfSplits :: Int
  , splAngle :: Double
  , sprAngle :: Double
  }
  deriving (Show, Generic)

type M g s a = Parameters -> g -> s -> (a, s, g)
                                            
newtype C g s r a = C { runC :: (a -> M g s r) -> M g s r }

instance Functor (C g s r) where
  fmap f m = C $ \ c -> runC m (c . f)
  {-# INLINE fmap #-}

instance Applicative (C g s r) where
  pure x = C ($ x)
  {-# INLINE pure #-}
  f <*> v = C $ \ c -> runC f $ \ g -> runC v (c . g)
  {-# INLINE (<*>) #-}
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance Monad (C g s r) where
  m >>= k = C $ \ c -> runC m (\ x -> runC (k x) c)
  {-# INLINE (>>=) #-}

instance MonadCont (C g s r) where
  callCC f = C $ \ c -> runC (f (\ x -> C $ \ _ -> c x)) c
  {-# INLINE callCC #-}

evalC :: C g s r r -> M g s r
evalC m = runC m (\ r -> \ _ g s -> (r, s, g))
{-# INLINE evalC #-}

liftMC :: M g s a -> C g s r a
liftMC m = C $ \ c -> \ p g s ->
  let ~(a, s', g') = m p g s
  in (c a) p g' s'
{-# INLINE liftMC #-}

instance MonadReader Parameters (C g s r) where
  ask = liftMC $ \ p g s -> (p, s, g)
  {-# INLINE ask #-}
  reader = liftMC . (\ f p g s -> (f p, s, g))
  {-# INLINE reader #-}
  local f m = C $ \ c -> \ p g s -> (runC m ((\ m' -> m' . (const p)) . c)) (f p) g s
  {-# INLINE local #-}

instance MonadState s (C g s r) where
  get = liftMC $ \ _ g s -> (s, s, g)
  {-# INLINE get #-}
  put = liftMC . (\ s _ g _ -> ((), s, g))
  {-# INLINE put #-}
  state = liftMC . (\ f _ g s -> let ~(a, s') = f s in (a, s', g))
  {-# INLINE state #-}

instance RandomGen g => MonadRandom (C g s r) where
  getRandomR = liftMC . (\ lohi _ g s -> let ~(a, g') = randomR lohi g in (a, s, g'))
  {-# INLINE getRandomR #-}
  getRandom = liftMC $ \ _ g s -> let ~(a, g') = random g in (a, s, g')
  {-# INLINE getRandom #-}
  getRandomRs = liftMC . (\ lohi _ g s -> let ~(as, g') = (first (randomRs lohi) . split) g
                                          in (as, s, g'))
  {-# INLINE getRandomRs #-}
  getRandoms = liftMC $ \ _ g s -> let ~(as, g') = (first randoms . split) g
                                   in (as, s, g')
  {-# INLINE getRandoms #-}

type T g = C g Tree

type MS g = C g MakeStem

runTinMS :: T g a a -> MS g r a
runTinMS f = C $ \ k -> \ p g MakeStem {..} ->
  let ~(r, Tree {..}, g') = (evalC f) p g Tree {..}
  in k r p g' MakeStem {..}

-- remove when undating to version 2.3.1 of mtl
label :: MonadCont m => a -> m (a -> m b, a)
label a = callCC $ \ k -> let go b = k (go, b) in return (go, a)
