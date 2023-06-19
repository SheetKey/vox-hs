{-# LANGUAGE RecordWildCards #-}

module Gox.Shape where

-- gox-hs
import Gox.Type
import Gox.Util

-- base
import Control.Applicative (liftA2)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear

-- lens
import Control.Lens.Getter ((^.))

data AABB = AABB
  { lx :: Int
  , ly :: Int
  , lz :: Int
  , ux :: Int
  , uy :: Int
  , uz :: Int
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
        numY = case dx `divMod` 16 of
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

class Bezier a where
  getRadius :: a -> Double
  compute :: a -> Double -> BezierPoint
  bezieraabb :: a -> AABB
  extrema :: a -> V3 [Double]

  bezieraabb bezier =
    let (V3 xPoints yPoints zPoints) = (fmap . fmap) (compute bezier) (extrema bezier)
        xs = ((^._x) . fst) <$> xPoints
        ys = ((^._y) . fst) <$> yPoints
        zs = ((^._z) . fst) <$> zPoints
    in AABB { lx = floor $ minimum xs
            , ly = floor $ minimum ys
            , lz = floor $ minimum zs
            , ux = ceiling $ maximum xs
            , uy = ceiling $ maximum ys
            , uz = ceiling $ maximum zs
            }

type BezierPoint = (V3 Double, Double)

data LinearBezier = LinearBezier
  { lx0 :: Int 
  , ly0 :: Int
  , lz0 :: Int
  , lx1 :: Int
  , ly1 :: Int
  , lz1 :: Int
  , lbR :: Int
  }

instance Bezier LinearBezier where
  getRadius = fromIntegral . lbR
  compute LinearBezier {..} tVal = case tVal of
    0 -> (fromIntegral <$> (V3 lx0 ly0 lz0), 0)
    1 -> (fromIntegral <$> (V3 lx1 ly1 lz1), 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  x = (mt * fromIntegral lx0) + (t * fromIntegral lx1)
                  y = (mt * fromIntegral ly0) + (t * fromIntegral ly1)
                  z = (mt * fromIntegral lz0) + (t * fromIntegral lz1)
              in (V3 x y z, t)
  bezieraabb LinearBezier {..} = AABB
    { lx = lx0 - 2 * lbR
    , ly = ly0 - 2 * lbR
    , lz = lz0 - 2 * lbR
    , ux = lx1 + 2 * lbR
    , uy = ly1 + 2 * lbR
    , uz = lz1 + 2 * lbR
    }
  extrema _ = V3 [0, 1] [0, 1] [0, 1]

linearBezierRoots :: LinearBezier -> V3 [Double]
linearBezierRoots LinearBezier {..} =
  let t a b = if a /= b then [a / (a - b)] else []
      tx = t (fromIntegral lx0) (fromIntegral lx1)
      ty = t (fromIntegral ly0) (fromIntegral ly1)
      tz = t (fromIntegral lz0) (fromIntegral lz1)
  in V3 tx ty tz

data QuadraticBezier = QuadraticBezier
  { qx0 :: Int
  , qy0 :: Int
  , qz0 :: Int
  , qx1 :: Int
  , qy1 :: Int
  , qz1 :: Int
  , qx2 :: Int
  , qy2 :: Int
  , qz2 :: Int
  , qbR :: Int
  }

instance Bezier QuadraticBezier where
  getRadius = fromIntegral . qbR
  compute QuadraticBezier {..} tVal = case tVal of
    0 -> (fromIntegral <$> (V3 qx0 qy0 qz0), 0)
    1 -> (fromIntegral <$> (V3 qx2 qy2 qz2), 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  mt2 = mt * mt
                  t2 = t * t
                  a = mt2
                  b = mt * t * 2
                  c = t2
                  x = (a * fromIntegral qx0) + (b * fromIntegral qx1) + (c * fromIntegral qx2)
                  y = (a * fromIntegral qy0) + (b * fromIntegral qy1) + (c * fromIntegral qy2)
                  z = (a * fromIntegral qz0) + (b * fromIntegral qz1) + (c * fromIntegral qz2)
              in (V3 x y z, t)
  extrema bezier =
    let deriv = quadraticBezierDeriv bezier
        dRoots = filter (\t -> 0 < t && t < 1) <$> linearBezierRoots deriv
    in (++ [0, 1]) <$> dRoots
        

-- the derivative of a quadratic bezier is a linear bezier
quadraticBezierDeriv :: QuadraticBezier -> LinearBezier
quadraticBezierDeriv QuadraticBezier {..} =
  let lx0 = 2 * (qx1 - qx0)
      ly0 = 2 * (qy1 - qy0)
      lz0 = 2 * (qz1 - qz0)
      lx1 = 2 * (qx2 - qx1)
      ly1 = 2 * (qy2 - qy1)
      lz1 = 2 * (qz2 - qz1)
      lbR = qbR
  in LinearBezier {..}

quadraticBezierRoots :: QuadraticBezier -> V3 [Double]
quadraticBezierRoots QuadraticBezier {..} =
  let t a b c = let d = a - (2 * b) + c
                in if d /= 0
                   then let m1 = - (sqrt $ (b * a) - (a * c))
                            m2 = - a + b
                            v1 = - (m1 + m2) / d
                            v2 = - (-m1 + m2) / d
                        in [v1, v2]
                   else if b /= c && d == 0
                        then [ (2 * b - c) / (2 * (b - c)) ]
                        else []
      tx = t (fromIntegral qx0) (fromIntegral qx1) (fromIntegral qx2)
      ty = t (fromIntegral qy0) (fromIntegral qy1) (fromIntegral qy2)
      tz = t (fromIntegral qz0) (fromIntegral qz1) (fromIntegral qz2)
  in V3 tx ty tz

data CubicBezier = CubicBezier
  { cx0 :: Int
  , cy0 :: Int
  , cz0 :: Int
  , cx1 :: Int
  , cy1 :: Int
  , cz1 :: Int
  , cx2 :: Int
  , cy2 :: Int
  , cz2 :: Int
  , cx3 :: Int
  , cy3 :: Int
  , cz3 :: Int
  , cbR :: Int
  }

instance Bezier CubicBezier where
  getRadius = fromIntegral . cbR
  compute CubicBezier {..} tVal = case tVal of
    0 -> (fromIntegral <$> (V3 cx0 cy0 cz0), 0)
    1 -> (fromIntegral <$> (V3 cx3 cy3 cz3), 1)
    t -> if not (0 < t && t < 1)
         then error "'t' not in bounds."
         else let mt = 1 - t
                  mt2 = mt * mt
                  t2 = t * t
                  a = mt2 * mt
                  b = mt2 * t * 3
                  c = mt * t2 * 3
                  d = t * t2
                  x = (a * fromIntegral cx0) + (b * fromIntegral cx1)
                    + (c * fromIntegral cx2) + (d * fromIntegral cx3)
                  y = (a * fromIntegral cy0) + (b * fromIntegral cy1)
                    + (c * fromIntegral cy2) + (d * fromIntegral cy3)
                  z = (a * fromIntegral cz0) + (b * fromIntegral cz1)
                    + (c * fromIntegral cz2) + (d * fromIntegral cz3)
              in (V3 x y z, t)
  extrema bezier =
    let deriv = cubicBezierDeriv bezier
        dderiv = quadraticBezierDeriv deriv
        dRoots = filter (\t -> 0 < t && t < 1) <$> quadraticBezierRoots deriv
        ddRoots = filter (\t -> 0 < t && t < 1) <$> linearBezierRoots dderiv
    in liftA2 ((++) . (++ [0, 1])) dRoots ddRoots

cubicBezierDeriv :: CubicBezier -> QuadraticBezier
cubicBezierDeriv CubicBezier {..} =
  let qx0 = 2 * (cx1 - cx0)
      qy0 = 2 * (cy1 - cy0)
      qz0 = 2 * (cz1 - cz0)
      qx1 = 2 * (cx2 - cx1)
      qy1 = 2 * (cy2 - cy1)
      qz1 = 2 * (cz2 - cz1)
      qx2 = 2 * (cx3 - cx2)
      qy2 = 2 * (cy3 - cy2)
      qz2 = 2 * (cz3 - cz2)
      lbR = cbR
  in QuadraticBezier {..}

getLUT :: Bezier a => Int -> a -> V.Vector BezierPoint
getLUT steps bezier = V.generate (steps + 1) $ \i ->
  let t = (fromIntegral i) / (fromIntegral steps)
  in compute bezier t

closest :: V3 Int -> V.Vector BezierPoint -> (Double, BezierPoint)
closest p = V.foldl f (2 ^ 50, (V3 0 0 0, 0))
  where
    f best@(bestDist, (bestPoint, _)) bp@(point, _) =
      let dist = distance bestPoint point
      in if dist < bestDist
         then (dist, bp)
         else best

pointInRange :: Bezier a => V3 Int -> a -> Bool
pointInRange point bezier =
  let lut = getLUT 100 bezier
      (lutDist, lutPoin) = closest point lut
  in if lutDist <= radius
     then True
     else case lutDist of
            0 -> f 0 10
            1 -> f (1 - (1 / 100)) 10
            t -> f (t - (1 / 100)) 20
  where 
    step = 0.1 / 100
    radius = getRadius bezier
    pointDouble = fromIntegral <$> point
    f t1 num = let tValues = V.enumFromStepN t1 step num
                   -- TODO: use V.minimumOn once vector package updates version in nix repo
                   --  V.minimumOn (\t -> distance (compute bezier t) point) tValues
                   bestT = V.minimum $
                           (\t -> distance (fst $ compute bezier t) pointDouble) <$> tValues
                   (bestPnt, _) = compute bezier bestT
               in distance pointDouble bestPnt <= radius

newtype BezierShape a = BezierShape a

instance Bezier a => Shape (BezierShape a) where
  getaabb (BezierShape a) = bezieraabb a
  containsPoint (BezierShape a) = flip pointInRange a
