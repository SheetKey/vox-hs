module Vox.Shape.AABB where

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
