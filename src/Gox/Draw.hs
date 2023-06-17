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

-- linear
import Linear

drawCubeFromBottomLeft
  :: Int32 -- ^ 'w' is in the x direction as in goxel editor
  -> Int32 -- ^ 'h' is in the y direction as in goxel editor
  -> Int32 -- ^ 'd' is in the z direction as in goxel editor
  -> V3 Int32 -- ^ coordinate of the bottom left point of the cube
  -> Int32 -- ^ material index
  -> Maybe String -- ^ maybe the name of the layer
  -> GoxFile
  -> GoxFile
drawCubeFromBottomLeft w h d pnt = undefined
--  case (w > 0, h > 0, d >0) of
--    (True, True, True) ->

drawCubeFromBottomLeftLAYR
  :: Int32 -- ^ 'w' is in the x direction as in goxel editor
  -> Int32 -- ^ 'h' is in the y direction as in goxel editor
  -> Int32 -- ^ 'd' is in the z direction as in goxel editor
  -> V3 Int32 -- ^ coordinate of the bottom left point of the cube
  -> Int32 -- ^ next free block index
  -> String -- ^ layrName
  -> Int32 -- ^ layrId
  -> Int32 -- ^ materialIdx
  -> (LAYR, V.Vector BL16)
drawCubeFromBottomLeftLAYR w h d (V3 x y z) layrName layrId materialIdx = undefined
--  ( LAYR
--    { mat = identity
--    , baseId = 0
--    , mImgPath = Nothing
--    , mBox = Nothing
--    , mShape = Nothing
--    , mColor = Nothing
--    , visible = 1
--    , blockData = undefined
--    , ..
--    }
--  ,
--    )

data AABB = AABB
  { lx :: Int
  , ly :: Int
  , lz :: Int
  , ux :: Int
  , uy :: Int
  , uz :: Int
  }

data PreBL16 = PreBL16
  { offset    :: V3 Int
  , preBlocks :: V.Vector Word8
  }

class Shape a where
  getaabb :: a -> AABB
  containsPoint :: a -> V3 Int -> Bool
  containedBy :: a -> [PreBL16]

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
  containedBy (Cube AABB {..}) =
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
    in [ PreBL16 (V3 (lx + x) (ly + y) (lz + z)) V.empty
       | x <- [0..(numX - 1)]
       , y <- [0..(numY - 1)]
       , z <- [0..(numZ - 1)] ]

fillBL16 :: Shape a => a -> [PreBL16]
fillBL16 a = undefined

vecIdxToVox :: Int -> V3 Int
vecIdxToVox = pngToVox . vecIdxToPngCoord

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
