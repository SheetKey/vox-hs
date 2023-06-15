{-# LANGUAGE RecordWildCards #-}

module Vox.Draw where

-- vox-hs
import Vox.Type

-- base
import Data.Int

-- vector
import qualified Data.Vector.Unboxed as U

drawCubeBottomLeft
  :: Int8 -- ^ length in x direction
  -> Int8 -- ^ length in y direction
  -> Int8 -- ^ length in z direction
  -> Int8 -- ^ min x coord
  -> Int8 -- ^ min y coord
  -> Int8 -- ^ min z coord
  -> Int8 -- ^ color index
  -> VoxFile
  -> VoxFile
drawCubeBottomLeft lx ly lz = drawShape ( cubeBottomLeft lx ly lz )

drawSphereFromCenter
  :: Int8 -- ^ radius
  -> Int8 -- ^ center x coord
  -> Int8 -- ^ center y coord
  -> Int8 -- ^ center z coord
  -> Int8 -- ^ color index
  -> VoxFile
  -> VoxFile
drawSphereFromCenter r = drawShape ( sphereFromCenter r )

drawShape
  :: (Int8 -> Int8 -> Int8 -> Int8 -> (Size, XYZI))
  -> Int8 -- ^ min x coord
  -> Int8 -- ^ min y coord
  -> Int8 -- ^ min z coord
  -> Int8 -- ^ color index
  -> VoxFile
  -> VoxFile
drawShape f ix iy iz colorIdx = addModel ( f ix iy iz colorIdx ) 

cubeBottomLeft
  :: Int8 -- ^ length in x direction
  -> Int8 -- ^ length in y direction
  -> Int8 -- ^ length in z direction
  -> Int8 -- ^ bottom left x coord
  -> Int8 -- ^ bottom left y coord
  -> Int8 -- ^ bottom left z coord
  -> Int8 -- ^ color index
  -> (Size, XYZI)
cubeBottomLeft lx ly lz ix iy iz colorIdx =
  if not $ lx > 0 && ly > 0 && lz > 0
  then error "one of 'lx', 'ly', or 'lz' is not greater than zero."
  else let numVoxels = (fromIntegral lx) * (fromIntegral ly) * (fromIntegral lz)
           voxels = U.fromList [ (ix + x, iy + y, iz + z, colorIdx)
                               | x <- [0..(lx-1)]
                               , y <- [0..(ly-1)]
                               , z <- [0..(lz-1)] ]
       in ( Size (fromIntegral lx) (fromIntegral ly) (fromIntegral lz)
          , XYZI {..}
          )

sphereFromCenter
  :: Int8 -- ^ radius
  -> Int8 -- ^ center x coord
  -> Int8 -- ^ center y coord
  -> Int8 -- ^ center z coord
  -> Int8 -- ^ color index
  -> (Size, XYZI)
sphereFromCenter r cx cy cz colorIdx =
  if r <= 0
  then error "'r' is not greater than 0."
  else let voxels = U.fromList [ (cx + x, cy + y, cz + z, colorIdx)
                               | x <- [(-r)..r]
                               , y <- [(-r)..r]
                               , z <- [(-r)..r]
                               , (x*x) + (y*y) + (z*z) <= (r*r) ]
           numVoxels = fromIntegral $ U.length voxels
           size = fromIntegral $ (2 * r) + 1
       in ( Size size size size
          , XYZI {..}
          )
