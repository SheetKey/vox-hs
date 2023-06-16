module Gox.Type where

data GoxFile = GoxFile
  { blocks :: V.Vector BL16
  , layers :: V.Vector LAYR
  }

data BL16 = BL16
  { voxelData :: Image PixelRGBA8
  }

data LAYR = LAYR
  { name :: String
  , mat  :: M44
  , _id  :: Int
  }

data Material = Material
  { materialName :: String
  , baseColor :: V4 Float
  , metallic :: Float
  , roughness :: Float
  , emission :: Float
  }
