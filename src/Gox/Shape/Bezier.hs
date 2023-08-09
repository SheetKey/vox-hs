{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Gox.Shape.Bezier where

-- gox-hs
import Gox.Shape
import Gox.Type
import Gox.Util
import Gox.Tree.Type

-- base
import Control.Applicative (liftA2)
import Control.Monad (join)
import Debug.Trace

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear hiding (vector, _x, _y, _z, trace)

-- hmatrix
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

-- optics-core
import Optics.Optic
import Optics.Lens
import Optics.Getter
import Data.Tuple.Optics

-- MonadRandom
import Control.Monad.Random.Lazy

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
                          in col
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

instance Shape (TaperedBezierCurve CubicBezier) where
  getaabb TaperedBezierCurve {..} = aabbEnlargeBy (bezieraabb taperedBezierCurve)
                                    (ceiling taperMaxRadius)
  containsPoint = pointInRangeTBCInt
  fullBL16 a =
    let preBL16s = emptyBL16 a
        mat = mkInversionMatrix (taperedBezierCurve a)
        v = fixInversion mat $ solveInversion mat
        inversion = mkInversion v
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
                     in if distSqrd point actualPoint <= 10 ^2
                        then 255
                        else 0
                else 0
         }

instance (Bezier a, Shape (f a)) => Shape (V.Vector (f a)) where
  getaabb = V.foldr (\ a box -> getaabb a <> box) mempty
  containsPoint v p = V.any ((flip containsPoint) p) v
  fullBL16 = join . fmap fullBL16

  drawShape v file = V.foldr' drawShape file v





-- new potential
-- generate a vector of only things very close to the curve: use this
-- to batch update a zero vetor

-- attempt 2

-- integer points in a sphere centered at origin of radius r
spherePoints :: Int -> V.Vector (V3 Int)
spherePoints r =
  let lower = negate r
      upper = r
      range = [lower..upper]
      unfilteredPoints = V.fromList [ V3 x y z
                                    | x <- range
                                    , y <- range
                                    , z <- range
                                    ]
  in V.filter (\ (V3 x y z) -> x*x + y*y + z*z <= r*r) unfilteredPoints

sphereAround :: (Double -> Double) -> (V3 Int, Double) -> V.Vector (V3 Int)
sphereAround f (pos, t) = 
  let r = f t
      sphere = spherePoints (ceiling r)
  in (+pos) <$> sphere

getLUT :: Bezier a => Int -> a -> V.Vector BezierPoint
getLUT steps bezier = V.generate (steps + 1) $ \i ->
  let t = (fromIntegral i) / (fromIntegral steps)
  in compute bezier t

lutToPoints :: V.Vector BezierPoint -> V.Vector (V3 Int, Double)
lutToPoints = fmap (\ (a, b) -> (fmap truncate a, b))

filterLut :: V.Vector (V3 Int) -> V3 Int -> V.Vector (V3 Int)
filterLut lut offset = V.filter
  (\ v ->
     let V3 x y z = v - offset
     in 0 <= x && x < 16
        && 0 <= y && y < 16
        && 0 <= z && z < 16
  ) lut

-- accepts what is named 'circledLut': the lut points with sheres centered at each point
partitionCircledLut :: V.Vector (V3 Int) -> V.Vector PreBL16
                     -> V.Vector ((V3 Int, V.Vector (V3 Int)))
partitionCircledLut circledLut preBL16s =
  let (rest, acc) = V.foldl
                    (\ (remCLut, acc) (PreBL16 offset _) ->
                       let (inRange, rem) = V.unstablePartition
                                            (\ v -> let V3 x y z = v - offset
                                                    in 0 <= x && x < 16
                                                       && 0 <= y && y < 16
                                                       && 0 <= z && z < 16
                                            ) remCLut
                       in if V.null inRange
                          then (rem, acc)
                          else (rem, (offset, inRange) `V.cons` acc)
                    ) (circledLut, V.empty) preBL16s
  in if not (V.null rest)
     -- then error "expected 'rest' to be empty when partitioning a 'circledLut'."
     then trace "doesn't fit in bl16" $ acc
     else acc

toBL16 :: (V3 Int, V.Vector (V3 Int)) -> PreBL16
toBL16 (offset, pnts) =
  let reps = concatMap
             (\ v -> let i = unsafeVoxToVecIdx (v - offset)
                     in [(i, 255), (i+1, 255), (i+2, 255), (i+3, 255)]
             ) pnts
      blocks = (VS.replicate 16384 0) VS.// reps
  in PreBL16 { offset = offset, preBlocks = blocks }

-- calcOffsets :: Double -> V3 Int -> V.Vector (V3 Int)
-- calcOffsets radius pnt =
--   let r = 1 + ceiling radius
--       minPnt = pnt - (V3 r r r)
--       V3 xMax yMax zMax = pnt + (V3 r r r)
--       V3 xoff yoff zoff = (\ x -> 16 * (floor $ fromIntegral x / 16)) <$> minPnt
--   in case (xoff + 15 >= xMax, yoff + 15 >= yMax, zoff + 15 >= zMax) of
--        (True, True, True)    -> V.fromList [ (V3  xoff        yoff              zoff) ]
--        (False, True, True)   -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3 (xoff + 16)  yoff              zoff) ]
--        (False, False, True)  -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3 (xoff + 16)  yoff              zoff)
--                                            , (V3  xoff       (yoff + 16)        zoff)
--                                            , (V3 (xoff + 16) (yoff + 16)        zoff) ]
--        (False, False, False) -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3 (xoff + 16)  yoff              zoff)
--                                            , (V3  xoff       (yoff + 16)        zoff)
--                                            , (V3  xoff        yoff             (zoff + 16))
--                                            , (V3 (xoff + 16) (yoff + 16)        zoff)
--                                            , (V3 (xoff + 16)  yoff             (zoff + 16))
--                                            , (V3  xoff       (yoff + 16)       (zoff + 16))
--                                            , (V3 (xoff + 16) (yoff + 16)       (zoff + 16)) ]
--        (False, True, False)  -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3 (xoff + 16)  yoff              zoff)
--                                            , (V3  xoff        yoff             (zoff + 16))
--                                            , (V3 (xoff + 16)  yoff             (zoff + 16)) ]
--        (True, False, True)   -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3  xoff       (yoff + 16)        zoff) ]
--        (True, False, False)  -> V.fromList [ (V3  xoff        yoff              zoff)
--                                            , (V3  xoff       (yoff + 16)        zoff)
--                                            , (V3  xoff        yoff              (zoff + 16))
--                                            , (V3  xoff       (yoff + 16)        (zoff + 16)) ]
--        (True, True, False)   -> V.fromList [ (V3  xoff        yoff               zoff)
--                                            , (V3  xoff        yoff              (zoff + 16)) ]

calcOffsets :: Double -> V3 Int -> V.Vector (V3 Int)
calcOffsets radius pnt =
  let r = 1 + ceiling radius
      minPnt = pnt - (V3 r r r)
      maxPnt = pnt + (V3 r r r)
      V3 lx ly lz = (\ x -> 16 * (floor $ fromIntegral x / 16)) <$> minPnt
      V3 ux uy uz = (\ x -> 16 * (ceiling $ fromIntegral x / 16)) <$> maxPnt
  in V.fromList [ V3 x y z
                | x <- [lx,(lx+16)..ux]
                , y <- [ly,(ly+16)..uy]
                , z <- [lz,(lz+16)..uz] ]

-- return vector of (offset, lutPoint)
offsetsWithPoints :: (Double -> Double) -> V.Vector (V3 Int, Double)
                  -> V.Vector (V3 Int, (V3 Int, Double))
offsetsWithPoints f = V.foldl (\ acc (pnt, t) ->
                                 let r = f t
                                     os = calcOffsets r pnt
                                     osPnts = (, (pnt, t)) <$> os
                                 in osPnts V.++ acc
                              ) V.empty

offsetPointsToBL16 :: (Double -> Double) -> (V3 Int, V.Vector (V3 Int, Double)) -> PreBL16
offsetPointsToBL16 f (offset, pnts) =
  let blocks = V.foldl (\ acc pnt ->
                          let circled = sphereAround f pnt
                              is = concatMap (\ v ->
                                                let V3 x y z = v - offset
                                                in if 0 <= x && x < 16
                                                      && 0 <= y && y < 16
                                                      && 0 <= z && z < 16
                                                   then let i = unsafeVoxToVecIdx (v - offset)
                                                        in [ (i, 255), (i+1, 255)
                                                           , (i+2, 255), (i+3, 255)]
                                                   else []
                                             ) circled
                          in acc VS.// is
                       ) (VS.replicate 16384 0) pnts
  in PreBL16 { offset = offset, preBlocks = blocks }

offsetPntsToBL16 :: (Double -> Double) -> V.Vector (V3 Int, (V3 Int, Double)) -> V.Vector PreBL16
offsetPntsToBL16 f = go V.empty
  where
    go acc v = case v V.!? 0 of
      Just (offset, _) ->
        let (offsetV, rest) = V.unstablePartition (\ (o, _) -> o == offset) v
            owps = (offset, snd <$> offsetV)
            bl16 = offsetPointsToBL16 f owps
        in go (bl16 `V.cons` acc) rest
      Nothing -> acc

pointToCircleBL16 :: (Double -> Double) -> (V3 Int, Double) -> PreBL16
pointToCircleBL16 f lutPnt@(pnt, _) = 
  let circled = sphereAround f lutPnt
      offset = pnt - (V3 8 8 8)
      is = concatMap
           (\ v -> let i = unsafeVoxToVecIdx (v - offset)
                   in [(i, 255), (i+1, 255), (i+2, 255), (i+3, 255)]
           ) circled
      blocks = (VS.replicate 16384 0) VS.// is
  in PreBL16 { offset = offset, preBlocks = blocks }

-- assumes that maxradius is less than or equal to 8: makes a PreBL16 for each lut point
lutToBL16s :: (Double -> Double) -> V.Vector (V3 Int, Double) -> V.Vector PreBL16
lutToBL16s f = fmap (pointToCircleBL16 f)


newtype TBC a = TBC (TaperedBezierCurve a)

instance Show a => Show (TBC (TaperedBezierCurve a)) where
  show (TBC c) = show c

instance Shape (TBC CubicBezier) where
  getaabb (TBC c) = getaabb c
  containsPoint (TBC (TaperedBezierCurve {..})) p = undefined
  fullBL16 (TBC a@(TaperedBezierCurve {..})) =
    let preBL16s = emptyBL16 a
        preBL16Num = V.length preBL16s
        preLut = lutToPoints $
                 getLUT (preBL16Num * (max 2 $ ceiling $ 16 / taperMaxRadius)) taperedBezierCurve
        osps = offsetsWithPoints taperingFunction preLut
    in offsetPntsToBL16 taperingFunction osps
    -- in trace (show (V.length preLut)) $ lutToBL16s taperingFunction preLut
    --     circledLut = V.concatMap (sphereAround taperingFunction) preLut
    --     partitioned = partitionCircledLut circledLut preBL16s
    -- in toBL16 <$> partitioned
  -- fullBL16 (TBC a@(TaperedBezierCurve {..})) =
  --   let preBL16s = emptyBL16 a
  --       preBL16Num = V.length preBL16s
  --       preLut = lutToPoints $
  --                getLUT (preBL16Num * (max 2 $ ceiling $ 16 / taperMaxRadius)) taperedBezierCurve
  --       circledLut = V.concatMap (sphereAround taperingFunction) preLut
  --   in trace (show preBL16Num) $ flip fmap preBL16s $
  --      \ (PreBL16 { offset = offset }) ->
  --        let lut = filterLut circledLut offset
  --        in case V.length lut of
  --             0 -> PreBL16 { offset = offset, preBlocks = VS.replicate 16384 0 }
  --             _ -> let points = concatMap
  --                               (\ v -> let i = unsafeVoxToVecIdx (v - offset)
  --                                       in [(i, 255) , (i+1, 255), (i+2, 255), (i+3, 255)]
  --                               ) lut
  --                      blocks = (VS.replicate 16384 0) VS.// points
  --                  in PreBL16
  --                     { offset = offset
  --                     , preBlocks = blocks
  --                     }

-- end attempt 2

-- pointsOnSphere :: Int -> V.Vector (V3 Double)
-- pointsOnSphere n = let g = mkStdGen 12345 in (flip evalRand) g $ 
--   V.replicateM n $ do 
--   u <- getRandomR (-1, 1)
--   t <- getRandomR (0, 2 * pi)
--   let s = sqrt $ 1 - u * u
--   return $ V3 (s * cos t) (s * sin t) u
-- 
-- surroundingCoords :: Double -> Int -> V.Vector (Int, Double)
-- surroundingCoords r i = 
--   fmap
--   ((, 255) . unsafeVoxToVecIdx . (+ unsafeVecIdxToVox i) . (fmap (truncate . (* r))))
--   (pointsOnSphere 20)
-- 
-- -- assumes already filtered of empty voxels
-- mkUpdateingVec :: Double -> V.Vector (Int, Double) -> V.Vector (Int, Double)
-- mkUpdateingVec r v = V.concatMap (\ (i, _) -> surroundingCoords r i) v
-- 
-- getLUT :: Bezier a => Int -> a -> V.Vector BezierPoint
-- getLUT steps bezier = V.generate (steps + 1) $ \i ->
--   let t = (fromIntegral i) / (fromIntegral steps)
--   in compute bezier t
-- 
-- lutToIdxed :: V.Vector BezierPoint -> V.Vector (Int, Double)
-- lutToIdxed = fmap ((, 255) . unsafeVoxToVecIdx . (fmap ceiling) . fst)


-- NEWEST new potential: implicit matrix form

mkS2 :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> Matrix Double
mkS2 (V3 x0 y0 z0) (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) =
  (6><12)
  [ 1,    0,   0,    x0,        0,        0,         y0,        0,        0,         z0,        0,        0
  , 3/5,  2/5, 0,    (x1*3)/5,  (x0*2)/5, 0,         (y1*3)/5,  (y0*2)/5, 0,         (z1*3)/5,  (z0*2)/5, 0
  , 3/10, 3/5, 1/10, (x2*3)/10, (x1*3)/5, x0/10,     (y2*3)/10, (y1*3)/5, y0/10,     (z2*3)/10, (z1*3)/5, z0/10
  , 1/10, 3/5, 3/10, x3/10,     (x2*3)/5, (x1*3)/10, y3/10,     (y2*3)/5, (y1*3/10), z3/10,     (z2*3)/5, (z1*3)/10
  , 0,    2/5, 3/5,  0,         (x3*2)/5, (x2*3)/5,  0,         (y3*2)/5, (y2*3)/5,  0,         (z3*2)/5, (z2*3)/5
  , 0,    0,   1,    0,         0,        x3,        0,         0,        y3,        0,         0,        z3
  ]

cubicS2 :: CubicBezier -> Matrix Double
cubicS2 CubicBezier {..} =
  (6><12)
  [ 1,    0,   0,    cx0,        0,         0,          cy0,        0,         0,          cz0,        0,         0
  , 3/5,  2/5, 0,    (cx1*3)/5,  (cx0*2)/5, 0,          (cy1*3)/5,  (cy0*2)/5, 0,          (cz1*3)/5,  (cz0*2)/5, 0
  , 3/10, 3/5, 1/10, (cx2*3)/10, (cx1*3)/5, cx0/10,     (cy2*3)/10, (cy1*3)/5, cy0/10,     (cz2*3)/10, (cz1*3)/5, cz0/10
  , 1/10, 3/5, 3/10, cx3/10,     (cx2*3)/5, (cx1*3)/10, cy3/10,     (cy2*3)/5, (cy1*3/10), cz3/10,     (cz2*3)/5, (cz1*3)/10
  , 0,    2/5, 3/5,  0,          (cx3*2)/5, (cx2*3)/5,  0,          (cy3*2)/5, (cy2*3)/5,  0,          (cz3*2)/5, (cz2*3)/5
  , 0,    0,   1,    0,          0,         cx3,        0,          0,         cy3,        0,          0,         cz3
  ]

mkS1 :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> Matrix Double
mkS1 (V3 x0 y0 z0) (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) =
  (5><8)
  [ 1  , 0  , x0      , 0       , y0      , 0       , z0      , 0              
  , 3/4, 1/4, (3*x1)/4, x0/4    , (3*y1)/4, y0/4    , (3*z1)/4, z0/4
  , 1/2, 1/2, x2/2    , x1/2    , y2/2    , y1/2    , z2/2    , z1/2
  , 1/4, 3/4, x3/4    , (3*x2)/4, y3/4    , (3*y2)/4, z3/4    , (3*z2)/4
  , 0  , 1  , 0       , x3      , 0       , y3      , 0       , z3
  ]

cubicS1 :: CubicBezier -> Matrix Double
cubicS1 CubicBezier {..} =
  (5><8)
  [ 1  , 0  , cx0      , 0        , cy0      , 0        , cz0      , 0              
  , 3/4, 1/4, (3*cx1)/4, cx0/4    , (3*cy1)/4, cy0/4    , (3*cz1)/4, cz0/4
  , 1/2, 1/2, cx2/2    , cx1/2    , cy2/2    , cy1/2    , cz2/2    , cz1/2
  , 1/4, 3/4, cx3/4    , (3*cx2)/4, cy3/4    , (3*cy2)/4, cz3/4    , (3*cz2)/4
  , 0  , 1  , 0        , cx3      , 0        , cy3      , 0        , cz3
  ]

-- expects the nullsapce of S1. (mkS1 and cubicS1 compute S1, NOT nullspace S1.
-- calling this on the output of either function can produce runtime matrix size errors.
-- instead call 'nullsapce' in between the computations.)
nullS1toM :: Matrix Double -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double)
nullS1toM nullS1 =
  let colNum = cols nullS1
      m0 = subMatrix (0, 0) (2, colNum) nullS1
      m1 = subMatrix (2, 0) (2, colNum) nullS1
      m2 = subMatrix (4, 0) (2, colNum) nullS1
      m3 = subMatrix (6, 0) (2, colNum) nullS1
  in (m0, m1, m2, m3)

nullS2toM :: Matrix Double -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double)
nullS2toM nullS2 =
  let colNum = cols nullS2
      m0 = subMatrix (0, 0) (3, colNum) nullS2
      m1 = subMatrix (3, 0) (3, colNum) nullS2
      m2 = subMatrix (6, 0) (3, colNum) nullS2
      m3 = subMatrix (9, 0) (3, colNum) nullS2
  in (m0, m1, m2, m3)

delta :: (Matrix Double, Matrix Double, Matrix Double, Matrix Double)
      -> V3 Double
      -> Double
delta (m0, m1, m2, m3) (V3 x y z) =
  let m0' = m0
      m1' = scale x m1
      m2' = scale y m2
      m3' = scale z m3
      m = m0' + m1' + m2' + m3'
      mT = tr' m
      --(_, (d, _)) = invlndet (m LA.<> mT)
  in det (m LA.<> mT)

cubicDeltaS1 :: CubicBezier -> V3 Double -> Double
cubicDeltaS1 = delta . nullS1toM . nullspace . cubicS1

cubicDeltaS2 :: CubicBezier -> V3 Double -> Double
cubicDeltaS2 = delta . nullS2toM . nullspace . cubicS2

newtype TBCS1 a = TBCS1 (TaperedBezierCurve a)

instance Show a => Show (TBCS1 (TaperedBezierCurve a)) where
  show (TBCS1 c) = show c

instance Shape (TBCS1 CubicBezier) where
  getaabb (TBCS1 c) = getaabb c
  containsPoint (TBCS1 (TaperedBezierCurve {..})) p =
    cubicDeltaS1 taperedBezierCurve (fromIntegral <$> p) <= taperMaxRadius * taperMaxRadius
  fullBL16 (TBCS1 a@(TaperedBezierCurve {..})) =
    let preBL16s = emptyBL16 a
        delta = cubicDeltaS1 taperedBezierCurve
        r = taperMaxRadius * taperMaxRadius
    in flip fmap preBL16s $
       \ (PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \ i ->
             let point = fmap fromIntegral $ offset + unsafeVecIdxToVox i
                 d = delta point
             in if d <= r then 255 else 0
         }

newtype TBCS2 a = TBCS2 (TaperedBezierCurve a)

instance Show a => Show (TBCS2 (TaperedBezierCurve a)) where
  show (TBCS2 c) = show c

instance Shape (TBCS2 CubicBezier) where
  getaabb (TBCS2 c) = getaabb c
  containsPoint (TBCS2 (TaperedBezierCurve {..})) p =
    cubicDeltaS2 taperedBezierCurve (fromIntegral <$> p) <= taperMaxRadius * taperMaxRadius
  fullBL16 (TBCS2 a@(TaperedBezierCurve {..})) =
    let preBL26s = emptyBL16 a
        delta = cubicDeltaS2 taperedBezierCurve
        r = taperMaxRadius * 1e9
    in flip fmap preBL26s $
       \ (PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \ i ->
             let point = fmap fromIntegral $ offset + unsafeVecIdxToVox i
                 d = delta point
             in if d ^ 2 <= r then 255 else 0
         }
