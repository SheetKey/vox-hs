{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Optics.Getter
import Optics.State.Operators

-- mtl
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont 
import Control.Monad.Cont.Class

-- linear
import Linear

-- vector
import qualified Data.Vector as V 

-- MonadRandom
import Control.Monad.Random.Lazy

-- containers
import qualified Data.Map.Strict as M

data BranchMode = Fan | Whorled | AltOpp

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
  , sIndex          :: Int
  }
  deriving (Show, Generic)

data Leaf = Leaf
  deriving Show

data Tree = Tree
  { tLeavesArray   :: Maybe (V.Vector Leaf)
  --, tStemIndex     :: Int
  , tTreeScale     :: Double
  --, tBranchCurves  :: V.Vector Curve
  , tBaseLength    :: Double
  , tSplitNumError :: V.Vector Double
  , tTrunkLength   :: Double
  , tStems         :: V.Vector Stem
  }
  deriving (Show, Generic)

class Wrappable a where
  getWType :: a -> WType a

instance Wrappable Double where
  getWType _ = WTypeDouble

instance Wrappable Int where
  getWType _ = WTypeInt

instance Wrappable Stem where
  getWType _ = WTypeStem

instance Wrappable Tree where
  getWType _ = WTypeTree

instance Wrappable Turtle where
  getWType _ = WTypeTurtle

instance Wrappable (V.Vector (Turtle, Turtle, Double, Double)) where
  getWType _ = WTypeBA

data WType a where
  WTypeDouble :: WType Double
  WTypeInt    :: WType Int
  WTypeStem   :: WType Stem
  WTypeTree   :: WType Tree
  WTypeTurtle :: WType Turtle
  WTypeBA     :: WType (V.Vector (Turtle, Turtle, Double, Double))
  
data Wrapper where
  Wrap :: Wrappable a => WType a -> a -> Wrapper

unwrapDouble :: Wrapper -> Double
unwrapDouble (Wrap wtype a) =
  case wtype of
    WTypeDouble -> a
    _ -> error "expected 'WTypeDouble'."

unwrapInt :: Wrapper -> Int
unwrapInt (Wrap wtype a) =
  case wtype of
    WTypeInt -> a
    _ -> error "expected 'WTypeInt'."

unwrapStem :: Wrapper -> Stem
unwrapStem (Wrap wtype a) =
  case wtype of
    WTypeStem -> a
    _ -> error "expected 'WTypeStem'."

unwrapTree :: Wrapper -> Tree
unwrapTree (Wrap wtype a) =
  case wtype of
    WTypeTree -> a
    _ -> error "expected 'WTypeTree'."
  
unwrapTurtle :: Wrapper -> Turtle
unwrapTurtle (Wrap wtype a) =
  case wtype of
    WTypeTurtle -> a
    _ -> error "expected 'WTypeTurtle'."

unwrapBA :: Wrapper -> V.Vector (Turtle, Turtle, Double, Double)
unwrapBA (Wrap wtype a) =
  case wtype of
    WTypeBA -> a
    _ -> error "expected 'WTypeBA'."

newtype M g a = M
  { runM
    :: Parameters
    -> g
    -> M.Map String Wrapper
    -> (a, M.Map String Wrapper, g)
  }

instance Functor (M g) where
  fmap f m = M $ \ p g sm ->
    let ~(a, sm', g') = runM m p g sm
    in (f a, sm', g')
  {-# INLINE fmap #-}

instance Applicative (M g) where
  pure a = M $ \ _ g sm -> (a, sm, g)
  {-# INLINE pure #-}
  mf <*> mv = M $ \ p g sm ->
    let ~(f, sm', g') = runM mf p g sm
        ~(v, sm'', g'') = runM mv p g' sm'
    in (f v, sm'', g'')
  {-# INLINE (<*>) #-}

instance Monad (M g) where
  m >>= f = M $ \ p g sm ->
    let ~(a, sm', g') = runM m p g sm
        ~(b, sm'', g'') = runM (f a) p g' sm'
    in (b, sm'', g'')
  {-# INLINE (>>=) #-}

instance RandomGen g => MonadRandom (M g) where
  getRandomR lohi = M $ \ _ g sm -> let (a, g') = randomR lohi g in (a, sm, g')
  getRandom = M $ \ _ g sm -> let (a, g') = random g in (a, sm, g')
  getRandomRs lohi = M $ \ _ g sm ->
                           let (as, g') = (first (randomRs lohi) . split) g
                           in (as, sm, g')
  getRandoms = M $ \ _ g sm ->
                     let (as, g') = (first randoms . split) g
                     in (as, sm, g')

getRandomState :: C g r g
getRandomState = lift $ M $ \ _ g sm -> (g, sm, g)

setRandomState :: g -> C g r ()
setRandomState g = lift $ M $ \ _ _ sm -> ((), sm, g)

instance MonadReader Parameters (M g) where
  ask = M $ \ p g sm -> (p, sm, g)
  {-# INLINE ask #-}
  reader f = M $ \ p g sm -> (f p, sm, g)
  {-# INLINE reader #-}
  local f m = M $ \ p g sm -> runM m (f p) g sm
  {-# INLINE local #-}

instance MonadState (M.Map String Wrapper) (M g) where
  get = M $ \ _ g sm -> (sm, sm, g)
  {-# INLINE get #-}
  put _ = error "'put' is unsafe for monad 'M g' and should not be used."
  {-# INLINE put #-}
  state f = M $ \ _ g sm -> let ~(a, sm') = f sm in (a, sm', g)
  {-# INLINE state #-}

type C g r a = ContT r (M g) a

evalC :: C g r r -> M g r
evalC m = runContT m return 
{-# INLINE evalC #-}

_getting :: String -> M.Map String Wrapper -> Wrapper
_getting str sm = case sm M.!? str of
  Nothing -> error $ "The current state does not contain a variable of name '" ++ str ++ "'."
  Just w -> w
{-# INLINE _getting #-}

unwrap :: WType a -> Wrapper -> a
unwrap WTypeDouble = unwrapDouble
unwrap WTypeInt = unwrapInt
unwrap WTypeStem = unwrapStem
unwrap WTypeTree = unwrapTree
unwrap WTypeTurtle = unwrapTurtle
unwrap WTypeBA = unwrapBA
{-# INLINE unwrap #-}

_getter :: WType a -> String -> M.Map String Wrapper -> a
_getter w str = unwrap w . _getting str
{-# INLINE _getter #-}

_setter :: Wrappable a => String -> M.Map String Wrapper -> a -> M.Map String Wrapper
_setter str sm a = if str `M.member` sm
  then M.insert str (Wrap (getWType a) a) sm
  else error $ "The current state does not contain a variable of name '" ++ str ++ "'."
{-# INLINE _setter #-}

_free :: String -> M g ()
_free str = M $ \ _ g sm -> if str `M.member` sm
  then ((), M.delete str sm, g)
  else error $ "The current state does not contain a variable of name '" ++ str ++ "'."
{-# INLINE _free #-}

newVar :: Wrappable a => String -> a -> C g r (Lens' (M.Map String Wrapper) a, C g r ())
newVar str a = do
  sm <- get
  if str `M.member` sm
    then error $ "The current state already contains a variable of name '" ++ str ++ "'."
    else do modify $ M.insert str (Wrap (getWType a) a) 
            return
              ( lens (_getter (getWType a) str) (_setter str)
              , lift $ _free str
              )

newStemVar :: TreeL -> Stem -> C g r StemL
newStemVar tree stem = do
  tree % #tStems %= (`V.snoc` stem)
  l <- uses (tree % #tStems) V.length
  return $ tree % #tStems % (unsafeVectorLens $ l-1)

-- remove when undating to version 2.3.1 of mtl
label :: MonadCont m => a -> m (a -> m b, a)
label a = callCC $ \ k -> let go b = k (go, b) in return (go, a)

type TreeL = Lens' (M.Map String Wrapper) Tree

type StemL = Lens' (M.Map String Wrapper) Stem

type TurtleL = Lens' (M.Map String Wrapper) Turtle

type DoubleL = Lens' (M.Map String Wrapper) Double

type IntL = Lens' (M.Map String Wrapper) Int

type BPL = Lens' (M.Map String Wrapper) BezierPoint

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do
  b <- mb
  when b thing 

uses :: (Is k A_Getter, MonadState s m) => Optic' k is s a -> (a -> b) -> m b
uses o f = gets (f . view o)

unsafeVectorLens :: Int -> Lens' (V.Vector a) a
unsafeVectorLens i = lens g s
  where
    g v = v V.! i
    s v a = v V.// [(i, a)]
