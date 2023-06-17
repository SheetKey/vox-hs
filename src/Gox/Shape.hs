{-# LANGUAGE RecordWildCards #-}

module Gox.Shape where

-- gox-hs
import Gox.Type
import Gox.Util

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear

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
