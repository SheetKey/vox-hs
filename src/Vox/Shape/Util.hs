module Vox.Shape.Utile where

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
