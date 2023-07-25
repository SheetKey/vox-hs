{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Gox.Shape where

-- gox-hs
import Gox.Type
import Gox.Util
import Gox.Tree.Type

-- base
import Control.Applicative (liftA2)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear hiding (_x, _y, _z)

-- lens
import Control.Lens.Getter ((^.))

-- optics-core
import Optics.Optic
import Optics.Lens
import Optics.Getter
import Data.Tuple.Optics

_x :: Lens' (V3 a) a
_x = lens (\ (V3 x _ _) -> x) (\ (V3 _ y z) x -> V3 x y z)
_y :: Lens' (V3 a) a
_y = lens (\ (V3 _ y _) -> y) (\ (V3 x _ z) y -> V3 x y z)
_z :: Lens' (V3 a) a
_z = lens (\ (V3 _ _ z) -> z) (\ (V3 x y _) z -> V3 x y z)

data AABB = AABB
  { lx :: Int
  , ly :: Int
  , lz :: Int
  , ux :: Int
  , uy :: Int
  , uz :: Int
  }
  deriving (Show)

instance Semigroup AABB where
  aabb1 <> aabb2 = AABB { lx = min (lx aabb1) (lx aabb2)
                        , ly = min (ly aabb1) (ly aabb2)
                        , lz = min (lz aabb1) (lz aabb2)
                        , ux = max (ux aabb1) (ux aabb2)
                        , uy = max (uy aabb1) (uy aabb2)
                        , uz = max (uz aabb1) (uz aabb2)
                        }

instance Monoid AABB where
  mempty = AABB 0 0 0 0 0 0

aabbEnlargeBy :: AABB -> Int -> AABB
aabbEnlargeBy AABB {..} r = AABB
  { lx = lx - r
  , ly = ly - r
  , lz = lz - r
  , ux = ux + r
  , uy = uy + r
  , uz = uz + r
  }

class Shape a where
  getaabb :: a -> AABB
  containsPoint :: a -> V3 Int -> Bool
  emptyBL16 :: a -> V.Vector PreBL16
  fullBL16 :: a -> V.Vector PreBL16
  drawShape :: a -> GoxFile -> GoxFile

  emptyBL16 a =
    let AABB {..} = getaabb a
        dx = ux - lx
        dy = uy - ly
        dz = uz - lz
        numX = case dx `divMod` 16 of
                 (d, 0) -> d
                 (d, r) -> d+1
        numY = case dy `divMod` 16 of
                 (d, 0) -> d
                 (d, r) -> d+1
        numZ = case dz `divMod` 16 of
                 (d, 0) -> d
                 (d, r) -> d+1
    in V.fromList [ PreBL16 (V3 (lx + (x * 16)) (ly + (y * 16)) (lz + (z * 16))) VS.empty
                  | x <- [0..(numX - 1)]
                  , y <- [0..(numY - 1)]
                  , z <- [0..(numZ - 1)] ]

  fullBL16 a =
    let preBL16s = emptyBL16 a
    in flip fmap preBL16s $
       \(PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \i ->
             if a `containsPoint` (offset + unsafeVecIdxToVox i)
             then 255
             else 0
         }

  drawShape = addLAYRfromBlocks . fullBL16

newtype Cube = Cube AABB

instance Shape Cube where
  getaabb (Cube aabb) = aabb
  containsPoint (Cube AABB {..}) (V3 x y z) =
    x >= lx
    && x <= ux
    && y >= ly
    && y <= uy
    && z >= lz
    && z <= uz

data Sphere = Sphere
  { sphereX :: Int
  , sphereY :: Int
  , sphereZ :: Int
  , sphereR :: Int
  }
  
instance Shape Sphere where
  getaabb Sphere {..} =
    let lx = sphereX - sphereR
        ly = sphereY - sphereR
        lz = sphereZ - sphereR
        ux = sphereX + sphereR
        uy = sphereY + sphereR
        uz = sphereZ + sphereR
    in AABB {..}
  containsPoint Sphere {..} (V3 x y z) =
    let dx = x - sphereX 
        dy = y - sphereY 
        dz = z - sphereZ 
    in (dx * dx) + (dy * dy) + (dz * dz) <= sphereR * sphereR

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
  --bezieraabb LinearBezier {..} = AABB
  --  { lx = lx0 
  --  , ly = ly0 
  --  , lz = lz0 
  --  , ux = lx1 
  --  , uy = ly1 
  --  , uz = lz1 
  --  }
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

instance Bezier (CurvePoint, CurvePoint) where
  type Deriv (CurvePoint, CurvePoint) = QuadraticBezier
  compute (bp1, bp2) tVal = case tVal of
    0 -> (bpControl bp1, 0)
    1 -> (bpControl bp2, 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  mt2 = mt * mt
                  t2 = t * t
                  a = mt2 * mt
                  b = mt2 * t * 3
                  c = mt * t2 * 3
                  d = t * t2
                  cx0 = view (#bpControl % _x) bp1
                  cy0 = view (#bpControl % _y) bp1
                  cz0 = view (#bpControl % _z) bp1
                  cx1 = view (#bpHandleRight % _x) bp1
                  cy1 = view (#bpHandleRight % _y) bp1
                  cz1 = view (#bpHandleRight % _z) bp1
                  cx2 = view (#bpHandleLeft % _x) bp2
                  cy2 = view (#bpHandleLeft % _y) bp2
                  cz2 = view (#bpHandleLeft % _z) bp2
                  cx3 = view (#bpControl % _x) bp2
                  cy3 = view (#bpControl % _y) bp2
                  cz3 = view (#bpControl % _z) bp2
                  x = (a * cx0) + (b * cx1)
                    + (c * cx2) + (d * cx3)
                  y = (a * cy0) + (b * cy1)
                    + (c * cy2) + (d * cy3)
                  z = (a * cz0) + (b * cz1)
                    + (c * cz2) + (d * cz3)
              in (V3 x y z, t)
  deriv (bp1, bp2) = 
    let cx0 = view (#bpControl % _x) bp1
        cy0 = view (#bpControl % _y) bp1
        cz0 = view (#bpControl % _z) bp1
        cx1 = view (#bpHandleRight % _x) bp1
        cy1 = view (#bpHandleRight % _y) bp1
        cz1 = view (#bpHandleRight % _z) bp1
        cx2 = view (#bpHandleLeft % _x) bp2
        cy2 = view (#bpHandleLeft % _y) bp2
        cz2 = view (#bpHandleLeft % _z) bp2
        cx3 = view (#bpControl % _x) bp2
        cy3 = view (#bpControl % _y) bp2
        cz3 = view (#bpControl % _z) bp2

        qx0 = 3 * (cx1 - cx0)
        qy0 = 3 * (cy1 - cy0)
        qz0 = 3 * (cz1 - cz0)
        qx1 = 3 * (cx2 - cx1)
        qy1 = 3 * (cy2 - cy1)
        qz1 = 3 * (cz2 - cz1)
        qx2 = 3 * (cx3 - cx2)
        qy2 = 3 * (cy3 - cy2)
        qz2 = 3 * (cz3 - cz2)
    in QuadraticBezier {..}
  extrema bezier =
    let quadDeriv = deriv bezier
        linDeriv = deriv quadDeriv
        dRoots = filter (\t -> 0 < t && t < 1) <$> quadraticBezierRoots quadDeriv
        ddRoots = filter (\t -> 0 < t && t < 1) <$> linearBezierRoots linDeriv
    in liftA2 ((++) . (++ [0, 1])) dRoots ddRoots

getLUT :: Bezier a => Int -> a -> V.Vector BezierPoint
getLUT steps bezier = V.generate (steps + 1) $ \i ->
  let t = (fromIntegral i) / (fromIntegral steps)
  in compute bezier t

closest :: V3 Int -> V.Vector BezierPoint -> (Double, BezierPoint)
closest p = V.foldl f (2 ^ 50, (V3 0 0 0, 0))
  where
    f best@(bestDist, _) bp@(point, _) =
      let dist = distance (fromIntegral <$> p) point
      in if dist < bestDist
         then (dist, bp)
         else best

pointInRangeLUT
  :: Bezier a => Int -> V.Vector BezierPoint -> a -> (Double -> Double) -> V3 Int -> Bool
pointInRangeLUT s lut bezier fTtoR point =
  let (lutDist, (_, lutT)) = closest point lut
  in if lutDist <= fTtoR lutT
     then True
     else case lutT of
            0 -> f 0 11
            1 -> f (1 - (1 / steps)) 11
            t -> f (t - (1 / steps)) 21
  where 
    steps = fromIntegral s
    step = 0.1 / steps
    pointDouble = fromIntegral <$> point
    f t1 num = let tValues = V.enumFromStepN t1 step num
                   -- TODO: use V.minimumOn once vector package updates version in nix repo
                   --  V.minimumOn (\t -> distance (compute bezier t) point) tValues
                   bestIdx = V.minIndex $
                           (\t -> distance (fst $ compute bezier t) pointDouble) <$> tValues
                   bestT = tValues V.! bestIdx
                   (bestPnt, _) = compute bezier bestT
               in distance pointDouble bestPnt <= fTtoR bestT

pointInRange :: Bezier a => a -> (Double -> Double) -> V3 Int -> Bool
pointInRange bezier = pointInRangeLUT 100 (getLUT 100 bezier) bezier 

data BezierCurve a = BezierCurve
  { bezierCurve :: a
  , bezierRadius :: Int
  }

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

data TaperedBezierCurve a = TaperedBezierCurve
  { taperedBezierCurve :: a
  , taperingFunction :: Double -> Double
  , taperMaxRadius :: Int
  }

instance Bezier a => Shape (TaperedBezierCurve a) where
  getaabb TaperedBezierCurve {..} = aabbEnlargeBy (bezieraabb taperedBezierCurve) taperMaxRadius
  containsPoint TaperedBezierCurve {..} = pointInRange taperedBezierCurve taperingFunction
  fullBL16 a =
    let preBL16s = emptyBL16 a
        lut = getLUT 100 $ taperedBezierCurve a
        aContainsPoint = pointInRangeLUT 100 lut (taperedBezierCurve a) (taperingFunction a)
    in flip fmap preBL16s $
       \(PreBL16 { offset = offset }) ->
         PreBL16
         { offset = offset
         , preBlocks = VS.generate 16384 $ \i ->
             if aContainsPoint (offset + unsafeVecIdxToVox i)
             then 255
             else 0
         }

linearTaper :: Double -> Double -> Double -> Double
linearTaper r1 r2 t = r1 + t * (r2 - r1)

instance Shape (CurvePoint, CurvePoint) where
  getaabb bps = aabbEnlargeBy (bezieraabb bps)
                (ceiling $ max (view (_1 % #bpRadius) bps) (view (_2 % #bpRadius) bps))
  containsPoint bps = pointInRange bps $ linearTaper 
                      (view (_1 % #bpRadius) bps) (view (_2 % #bpRadius) bps)

curvePointsToPairs :: Curve -> V.Vector (CurvePoint, CurvePoint)
curvePointsToPairs (Curve bps) = V.zip (V.init bps) (V.tail bps)

instance Shape Curve where
  getaabb curve = V.foldr (\ bp box -> getaabb bp <> box) mempty (curvePointsToPairs curve)
  containsPoint curve p = V.any ((flip containsPoint) p) (curvePointsToPairs curve)
