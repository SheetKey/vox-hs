module Vox.Shape.Class where

-- gox-hs
import Vox.Gox
import Vox.Tree.Type

-- base
import Control.Applicative (liftA2)
import Control.Monad (join)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

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
