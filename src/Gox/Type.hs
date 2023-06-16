{-# LANGUAGE RecordWildCards #-}

module Gox.Type where

-- vector
import qualified Data.Vector as V

-- linear
import Linear

-- binary
import Data.Binary.Get

-- JuicyPixels
import Codec.Picture.Types
import Codec.Picture.Png

data GoxFile = GoxFile
  { blocks :: V.Vector BL16
  , layers :: V.Vector LAYR
  , materials :: V.Vector Material
  }

data GoxChunk = GBL16 BL16
              | GLAYR LAYR
              | GMaterial Material
              | Skipped

data BL16 = BL16
  { voxelData :: Image PixelRGBA8
  }

data LAYR = LAYR
  { name :: String
  , mat  :: M44 Float
  , _id  :: Int
  }

data Material = Material
  { materialName :: String
  , baseColor :: V4 Float
  , metallic :: Float
  , roughness :: Float
  , emission :: Float
  }

type ParseError = (ByteOffset, String)

fromChunk :: GoxChunk -> GoxFile
fromChunk (GBL16 c) = GoxFile (V.singleton c) V.empty V.empty
fromChunk (GLAYR c) = GoxFile V.empty (V.singleton c) V.empty
fromChunk (GMaterial c) = GoxFile V.empty V.empty (V.singleton c)
fromChunk Skipped = GoxFile V.empty V.empty V.empty

addChunk :: GoxChunk -> GoxFile -> GoxFile
addChunk (GBL16 c) GoxFile {..} = GoxFile { blocks = blocks `V.snoc` c, .. }
addChunk (GLAYR c) GoxFile {..} = GoxFile { layers = layers `V.snoc` c, .. }
addChunk (GMaterial c) GoxFile {..} = GoxFile { materials = materials `V.snoc` c, .. }
addChunk Skipped goxFile = goxFile
