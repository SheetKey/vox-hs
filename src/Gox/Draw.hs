{-# LANGUAGE RecordWildCards #-}

module Gox.Draw where

-- gox-hs
import Gox.Type
import Gox.Util

-- base
import Data.Int
import Data.Word
import Data.Tuple (swap)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear

drawShape :: Shape a => a -> GoxFile -> GoxFile
drawShape = addLAYRfromBlocks . fullBL16

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
  emptyBL16 (Cube AABB {..}) =
    let dx = ux - lx
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

unsafeVecIdxToVox :: Int -> V3 Int
unsafeVecIdxToVox = unsafePngToVox . unsafeVecIdxToPngCoord

vecIdxToVox :: Int -> V3 Int
vecIdxToVox = pngToVox . vecIdxToPngCoord

unsafeVecIdxToPngCoord :: Int -> (Int, Int)
unsafeVecIdxToPngCoord i =
  let r = i `mod` 4
      idx = i - r
      unscaledIdx = idx `div` 4
      (y, x) = unscaledIdx `divMod` 64
  in (x, y)

vecIdxToPngCoord :: Int -> (Int, Int)
vecIdxToPngCoord i =
  if not ( 0 <= i && i < 16384 )
  then error "'i' not in bounds."
  else 
    let r = i `mod` 4
        idx = i - r    -- base index (index of r if 'i' was for r, g, b, or a)
        unscaledIdx = case idx `divMod` 4 of
                        (d, 0) -> d
                        (_, r) -> error $ "Expected remainder '0' but got '" ++ show r ++ "'."
        (x, y) = let (y, r) = unscaledIdx `divMod` 64
                 in if not (0 <= y && y < 64)
                    then error $ "Expected '0 <= y && y < 64' but got 'y == " ++ show y ++ "'."
                    else if not (0 <= r && r < 64)
                         then error $ "Expected '0 <= r && r < 64' but got 'r == " ++ show r ++ "'."
                         else (r, y)
    in (x, y)

unsafePngToVox :: (Int, Int) -> V3 Int
unsafePngToVox (r, d) =
  let p = d * 64 + r
      (z, xy) = p `divMod` 256
      (y, x) = xy `divMod` 16
  in V3 x y z

pngToVox :: (Int, Int) -> V3 Int
pngToVox (r, d) =
  if not ( 0 <= r && r < 64
           && 0 <= d && d < 64 )
  then error "'r' or 'd' not in bounds."
  else
    let p = d * 64 + r
        (xy, z) = let (z, xy) = p `divMod` 256
                  in if not (0 <= z && z < 64)
                     then error $ "Expected '0 <= z && z < 64' but got '" ++ show z ++ "'."
                     else (xy, z)
        (x, y) = let (y, x) = xy `divMod` 16
                 in if not (0 <= y && y < 16)
                    then error $ "Expected '0 <= y && y < 16' but got '" ++ show y ++ "'."
                    else if not (0 <= x && x < 16)
                         then error $ "Expected '0 <= x && x < 16' but got '" ++ show x ++ "'."
                         else (x, y)
    in V3 x y z

voxToVecIdx :: V3 Int -> Int
voxToVecIdx v@(V3 x y z) =
  if not 
     ( 0 <= x
       && x < 16
       && 0 <= y
       && y < 16
       && 0 <= z
       && z < 16 )
  then error "coord not in bounds"
  else unsafeVoxToVecIdx v

unsafeVoxToVecIdx :: V3 Int -> Int
unsafeVoxToVecIdx = unsafePngToVecIdx . unsafeVoxToPngCoord

voxToPngCoord :: V3 Int -> (Int, Int)
voxToPngCoord v@(V3 x y z) =
  if not
     ( 0 <= x
       && x < 16
       && 0 <= y
       && y < 16
       && 0 <= z
       && z < 16 )
  then error "coord not in bounds"
  else unsafeVoxToPngCoord v

unsafeVoxToPngCoord :: V3 Int -> (Int, Int)
unsafeVoxToPngCoord (V3 x y z) = swap $ (x + (y * 16) + (z * 256)) `divMod` 64

pngToVecIdx :: (Int, Int) -> Int
pngToVecIdx v@(x, y) =
  if not
     ( 0 <= x
       && x < 64
       && 0 <= y
       && y < 64
     )
  then error "coord not in bounds"
  else unsafePngToVecIdx v

unsafePngToVecIdx :: (Int, Int) -> Int
unsafePngToVecIdx (x, y) = (x + y * 64) * 4
