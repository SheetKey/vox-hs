{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Gox.Shape.Bezier where

-- gox-hs
import Gox.Shape
import Gox.Type
import Gox.Util
import Gox.Tree.Type

-- base
import Debug.Trace
import Control.Applicative (liftA2)
import Control.Monad (join)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear hiding (vector, _x, _y, _z, trace)

-- hmatrix
import Numeric.LinearAlgebra hiding ((<>))

-- optics-core
import Optics.Optic
import Optics.Lens
import Optics.Getter
import Data.Tuple.Optics

coef0 :: CubicBezier -> V3 Double
coef0 CubicBezier {..} = V3 cx0 cy0 cz0

coef1 :: CubicBezier -> V3 Double
coef1 CubicBezier {..} = V3 (-3 * cx0 + 3 * cx1)
                         (-3 * cy0 + 3 * cy1)
                         (-3 * cz0 + 3 * cz1)

coef2 :: CubicBezier -> V3 Double
coef2 CubicBezier {..} = V3 (3 * cx0 - 6 * cx1 + 3 * cx2)
                         (3 * cy0 - 6 * cy1 + 3 * cy2)
                         (3 * cz0 - 6 * cz1 + 3 * cz2)

coef3 :: CubicBezier -> V3 Double
coef3 CubicBezier {..} = V3 (negate cx0 + 3 * cx1 - 3 * cx2 + cx3)
                         (negate cy0 + 3 * cy1 - 3 * cy2 + cy3)
                         (negate cz0 + 3 * cz1 - 3 * cz2 + cz3)

inversionMatrix :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> Matrix Double
inversionMatrix (V3 x0 y0 z0) (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) =
  matrix 6 [ x3, y3, z3, 0,   0,   0
           , x2, y2, z2, -x3, -y3, -z3
           , x1, y1, z1, -x2, -y2, -z2
           , x0, y0, z0, -x1, -y1, -z1
           , 0,  0,  0,  -x0, -y0, -z0 ]

mkInversionMatrix :: CubicBezier -> Matrix Double
mkInversionMatrix cb = inversionMatrix (coef0 cb) (coef1 cb) (coef2 cb) (coef3 cb)

solveInversion :: Matrix Double -> Vector Double
solveInversion mat = flatten $ linearSolveSVD mat (reshape 1 (vector [0, 0, 0, 0, 0]))

fixInversion :: Matrix Double -> Vector Double -> Vector Double
fixInversion mat v = if v == vector [0, 0, 0, 0, 0, 0]
                     then let nullmat = nullspace mat
                              (col:_) = toColumns $ nullspace mat
                          in trace (show nullmat) col
                     else v

mkInversion :: Vector Double -> (V3 Double -> Double)
mkInversion v = if size v == 6
  then \ (V3 x y z) -> let a1 = v ! 0
                           b1 = v ! 1
                           c1 = v ! 2
                           a2 = v ! 3
                           b2 = v ! 4
                           c2 = v ! 5
                       in (a2 * x + b2 * y + c2 * z) / (a1 * x + b2 * x + c2 * x)
  else error "inversion vector is not of length 6"

type BezierPoint = (V3 Double, Double)

class Bezier a where
  type Deriv a
  compute :: a -> Double -> BezierPoint
  bezieraabb :: a -> AABB
  extrema :: a -> V3 [Double]
  deriv :: a -> Deriv a

  bezieraabb bezier =
    let (V3 xPoints yPoints zPoints) = (fmap . fmap) (compute bezier) (extrema bezier)
        xs = ((view _x) . fst) <$> xPoints
        ys = ((view _y) . fst) <$> yPoints
        zs = ((view _z) . fst) <$> zPoints
    in AABB { lx = floor $ minimum xs 
            , ly = floor $ minimum ys
            , lz = floor $ minimum zs
            , ux = ceiling $ maximum xs
            , uy = ceiling $ maximum ys
            , uz = ceiling $ maximum zs
            }

data LinearBezier = LinearBezier
  { lx0 :: Double 
  , ly0 :: Double
  , lz0 :: Double
  , lx1 :: Double
  , ly1 :: Double
  , lz1 :: Double
  }

instance Bezier LinearBezier where
  type Deriv LinearBezier = Double
  compute LinearBezier {..} tVal = case tVal of
    0 -> (V3 lx0 ly0 lz0, 0)
    1 -> (V3 lx1 ly1 lz1, 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  x = (mt * lx0) + (t * lx1)
                  y = (mt * ly0) + (t * ly1)
                  z = (mt * lz0) + (t * lz1)
              in (V3 x y z, t)
  extrema _ = V3 [0, 1] [0, 1] [0, 1]
  deriv _ = 0

linearBezierRoots :: LinearBezier -> V3 [Double]
linearBezierRoots LinearBezier {..} =
  let t a b = if a /= b then [a / (a - b)] else []
      tx = t lx0 lx1
      ty = t ly0 ly1
      tz = t lz0 lz1
  in V3 tx ty tz

data QuadraticBezier = QuadraticBezier
  { qx0 :: Double
  , qy0 :: Double
  , qz0 :: Double
  , qx1 :: Double
  , qy1 :: Double
  , qz1 :: Double
  , qx2 :: Double
  , qy2 :: Double
  , qz2 :: Double
  }

instance Bezier QuadraticBezier where
  type Deriv QuadraticBezier = LinearBezier
  compute QuadraticBezier {..} tVal = case tVal of
    0 -> (V3 qx0 qy0 qz0, 0)
    1 -> (V3 qx2 qy2 qz2, 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  mt2 = mt * mt
                  t2 = t * t
                  a = mt2
                  b = mt * t * 2
                  c = t2
                  x = (a * qx0) + (b * qx1) + (c * qx2)
                  y = (a * qy0) + (b * qy1) + (c * qy2)
                  z = (a * qz0) + (b * qz1) + (c * qz2)
              in (V3 x y z, t)
  extrema bezier =
    let linDeriv = deriv bezier
        dRoots = filter (\t -> 0 < t && t < 1) <$> linearBezierRoots linDeriv
    in (++ [0, 1]) <$> dRoots
  deriv QuadraticBezier {..} = 
    let lx0 = 2 * (qx1 - qx0)
        ly0 = 2 * (qy1 - qy0)
        lz0 = 2 * (qz1 - qz0)
        lx1 = 2 * (qx2 - qx1)
        ly1 = 2 * (qy2 - qy1)
        lz1 = 2 * (qz2 - qz1)
    in LinearBezier {..}

quadraticBezierRoots :: QuadraticBezier -> V3 [Double]
quadraticBezierRoots QuadraticBezier {..} =
  let t a b c = let d = a - (2 * b) + c
                in if d /= 0
                   then let m1 = - (sqrt $ (b * b) - (a * c))
                            m2 = - a + b
                            v1 = - (m1 + m2) / d
                            v2 = - (-m1 + m2) / d
                        in [v1, v2]
                   else if b /= c && d == 0
                        then [ (2 * b - c) / (2 * (b - c)) ]
                        else []
      tx = t qx0 qx1 qx2
      ty = t qy0 qy1 qy2
      tz = t qz0 qz1 qz2
  in V3 tx ty tz

data CubicBezier = CubicBezier
  { cx0 :: Double
  , cy0 :: Double
  , cz0 :: Double
  , cx1 :: Double
  , cy1 :: Double
  , cz1 :: Double
  , cx2 :: Double
  , cy2 :: Double
  , cz2 :: Double
  , cx3 :: Double
  , cy3 :: Double
  , cz3 :: Double
  }
  deriving (Show)

instance Bezier CubicBezier where
  type Deriv CubicBezier = QuadraticBezier
  compute CubicBezier {..} tVal = case tVal of
    0 -> (V3 cx0 cy0 cz0, 0)
    1 -> (V3 cx3 cy3 cz3, 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  mt2 = mt * mt
                  t2 = t * t
                  a = mt2 * mt
                  b = mt2 * t * 3
                  c = mt * t2 * 3
                  d = t * t2
                  x = (a * cx0) + (b * cx1)
                    + (c * cx2) + (d * cx3)
                  y = (a * cy0) + (b * cy1)
                    + (c * cy2) + (d * cy3)
                  z = (a * cz0) + (b * cz1)
                    + (c * cz2) + (d * cz3)
              in (V3 x y z, t)
  extrema bezier =
    let quadDeriv = deriv bezier
        linDeriv = deriv quadDeriv
        dRoots = filter (\t -> 0 < t && t < 1) <$> quadraticBezierRoots quadDeriv
        ddRoots = filter (\t -> 0 < t && t < 1) <$> linearBezierRoots linDeriv
    in liftA2 ((++) . (++ [0, 1])) dRoots ddRoots
  deriv CubicBezier {..} = 
    let qx0 = 3 * (cx1 - cx0)
        qy0 = 3 * (cy1 - cy0)
        qz0 = 3 * (cz1 - cz0)
        qx1 = 3 * (cx2 - cx1)
        qy1 = 3 * (cy2 - cy1)
        qz1 = 3 * (cz2 - cz1)
        qx2 = 3 * (cx3 - cx2)
        qy2 = 3 * (cy3 - cy2)
        qz2 = 3 * (cz3 - cz2)
    in QuadraticBezier {..}

data TaperedBezierCurve a = TaperedBezierCurve
  { taperedBezierCurve :: a
  , taperingFunction :: Double -> Double
  , taperMaxRadius :: Double
  }

instance Show a => Show (TaperedBezierCurve a) where
  show c = "TaperedBezierCurve {"
           ++ show (taperedBezierCurve c) ++ ", "
           ++ show (taperMaxRadius c) ++ "}"

linearTaper :: Double -> Double -> Double -> Double
linearTaper r1 r2 t = r1 + t * (r2 - r1)

curvePointsToPairs :: Curve -> V.Vector (CurvePoint, CurvePoint)
curvePointsToPairs (Curve bps) = V.zip (V.init bps) (V.tail bps)

pairToCubicBezier :: CurvePoint -> CurvePoint -> CubicBezier
pairToCubicBezier (CurvePoint (V3 cx0 cy0 cz0) _ (V3 cx1 cy1 cz1) _)
                  (CurvePoint (V3 cx3 cy3 cz3) (V3 cx2 cy2 cz2) _ _) = CubicBezier {..}

pairToTapered :: (CurvePoint, CurvePoint) -> TaperedBezierCurve CubicBezier
pairToTapered (cp1, cp2) = TaperedBezierCurve
  { taperedBezierCurve = pairToCubicBezier cp1 cp2
  , taperingFunction = linearTaper (bpRadius cp1) (bpRadius cp2)
  , taperMaxRadius = max (bpRadius cp1) (bpRadius cp2)
  }
  
pairsToTapered :: V.Vector (CurvePoint, CurvePoint) -> V.Vector (TaperedBezierCurve CubicBezier)
pairsToTapered = fmap pairToTapered

curveToTapered :: Curve -> V.Vector (TaperedBezierCurve CubicBezier)
curveToTapered = pairsToTapered . curvePointsToPairs

curvesToTapered :: V.Vector Curve -> V.Vector (TaperedBezierCurve CubicBezier)
curvesToTapered = V.foldr (\ a v -> curveToTapered a V.++ v) V.empty

treeToTapered :: Tree -> V.Vector (TaperedBezierCurve CubicBezier)
treeToTapered = curvesToTapered . treeToCurves

pointInRangeTBC :: TaperedBezierCurve CubicBezier -> V3 Double -> Bool
pointInRangeTBC TaperedBezierCurve {..} point =
  let mat = mkInversionMatrix taperedBezierCurve
      v = fixInversion mat $ solveInversion mat
      t = mkInversion v point
  in if 0 <= t && t <= 1
     then let (actualPoint, _) = compute taperedBezierCurve t
              r = taperingFunction t
          in distSqrd point actualPoint <= r * r
     else False

pointInRangeTBCInt :: TaperedBezierCurve CubicBezier -> V3 Int -> Bool
pointInRangeTBCInt tbc p = pointInRangeTBC tbc (fromIntegral <$> p)

distSqrd :: V3 Double -> V3 Double -> Double
distSqrd (V3 x1 y1 z1) (V3 x2 y2 z2) = (x1 - x2) * (x1 - x2)
                                       + (y1 - y2) * (y1 - y2)
                                       + (z1 - z2) * (z1 - z2)

instance Bezier a => Shape (BezierCurve a) where
  getaabb BezierCurve {..} = aabbEnlargeBy (bezieraabb bezierCurve) bezierRadius
  containsPoint BezierCurve {..} = pointInRange bezierCurve (\_ -> fromIntegral bezierRadius)
  fullBL16 a =
    let preBL16s = emptyBL16 a
        lut = getLUT 100 $ bezierCurve a
        aContainsPoint =
          pointInRangeLUT 100 lut (bezierCurve a) (\_ -> fromIntegral $ bezierRadius a)
    in flip fmap preBL16s $
       \(PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \i ->
             if aContainsPoint (offset + unsafeVecIdxToVox i)
             then 255
             else 0
         }

instance Shape (TaperedBezierCurve CubicBezier) where
  getaabb TaperedBezierCurve {..} = aabbEnlargeBy (bezieraabb taperedBezierCurve)
                                    (ceiling taperMaxRadius)
  containsPoint = pointInRangeTBCInt
  fullBL16 a =
    let preBL16s = emptyBL16 a
        mat = mkInversionMatrix (taperedBezierCurve a)
        v = trace (show mat) $ fixInversion mat $ solveInversion mat
        inversion = trace (show v) $ mkInversion v
    in flip fmap preBL16s $
       \ (PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \i -> 
             let point = fmap fromIntegral $ offset + unsafeVecIdxToVox i
                 t = inversion point
             in if 0 <= t && t <= 1
                then let r = (taperingFunction a) t
                         (actualPoint, _) = compute (taperedBezierCurve a) t
                     in if distSqrd point actualPoint <= r * r
                        then 255
                        else 0
                else 0
         }

instance (Bezier a, Shape (f a)) => Shape (V.Vector (f a)) where
  getaabb = V.foldr (\ a box -> getaabb a <> box) mempty
  containsPoint v p = V.any ((flip containsPoint) p) v
  fullBL16 = join . fmap fullBL16

  drawShape v file = V.foldr' drawShape file v

-- old stuff below

getLUT :: Bezier a => Int -> a -> V.Vector BezierPoint
getLUT steps bezier = V.generate (steps + 1) $ \i ->
  let t = (fromIntegral i) / (fromIntegral steps)
  in compute bezier t

closest :: V3 Int -> V.Vector BezierPoint -> (Double, BezierPoint)
closest p = V.foldl' f (2 ^ 50, (V3 0 0 0, 0))
  where
    V3 x y z = fromIntegral <$> p
    f best@(bestDist, _) bp@(V3 a b c, _) =
      let dist = (x - a) * (x - a) + (y - b) * (y - b) + (z - c) * (z - c)
      in if dist < bestDist
         then (dist, bp)
         else best

pointInRangeLUT
  :: Bezier a => Int -> V.Vector BezierPoint -> a -> (Double -> Double) -> V3 Int -> Bool
pointInRangeLUT s lut bezier fTtoR point =
  let (!lutDist, (_, !lutT)) = closest point lut
  in if lutDist <= fTtoR lutT
     then True
     else case lutT of
            0 -> f 0 11
            1 -> f (1 - (1 / steps)) 11
            t -> f (t - (1 / steps)) 21
  where 
    steps = fromIntegral s
    step = 0.1 / steps
    V3 px py pz = fromIntegral <$> point
    f t1 num = let tValues = V.enumFromStepN t1 step num
                   -- TODO: use V.minimumOn once vector package updates version in nix repo
                   --  V.minimumOn (\t -> distance (compute bezier t) point) tValues
                   bestIdx = V.minIndex $
                           (\t -> let V3 x y z = fst $ compute bezier t
                                  in (x - px) * (x - px) + (y - py) * (y - py) + (z - pz) * (z - pz)
                           ) <$> tValues
                   bestT = tValues V.! bestIdx
                   (V3 x y z, _) = compute bezier bestT
                   d = (x - px) * (x - px) + (y - py) * (y - py) + (z - pz) * (z - pz)
               in d <= (fTtoR bestT) * (fTtoR bestT)

pointInRange :: Bezier a => a -> (Double -> Double) -> V3 Int -> Bool
pointInRange bezier = pointInRangeLUT 100 (getLUT 100 bezier) bezier 

data BezierCurve a = BezierCurve
  { bezierCurve :: a
  , bezierRadius :: Int
  }
  deriving (Show)
