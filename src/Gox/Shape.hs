{-# LANGUAGE BangPatterns #-}
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
import Control.Monad (join)

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
{-# INLINE _x #-}

_y :: Lens' (V3 a) a
_y = lens (\ (V3 _ y _) -> y) (\ (V3 x _ z) y -> V3 x y z)
{-# INLINE _y #-}

_z :: Lens' (V3 a) a
_z = lens (\ (V3 _ _ z) -> z) (\ (V3 x y _) z -> V3 x y z)
{-# INLINE _z #-}

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

