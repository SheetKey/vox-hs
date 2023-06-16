{-# LANGUAGE RecordWildCards #-}

module Gox.Type where

-- base
import Data.Word
import Data.Int

-- vector
import qualified Data.Vector as V

-- linear
import Linear

-- binary
import Data.Binary.Get

-- JuicyPixels
import Codec.Picture.Types
import Codec.Picture.Png

type ChunkSize = Word32

data GoxFile = GoxFile
  { blocks :: V.Vector BL16
  , layers :: V.Vector LAYR
  , materials :: V.Vector Material
  }
  deriving (Show)

data GoxChunk = GBL16 BL16
              | GLAYR LAYR
              | GMaterial Material
              | Skipped
              | None
              deriving (Show)

data BL16 = BL16
  { voxelData :: Image PixelRGBA8
  }
instance Show BL16 where
  show _ = "BL16"

data LAYR = LAYR
  { layrName    :: String
  , mat         :: M44 Float
  , layrId      :: Int32
  , baseId      :: Int32
  , materialIdx :: Int32
  , mImgPath    :: Maybe String
  , mBox        :: Maybe (M44 Float)
  , mShape      :: Maybe String
  , mColor      :: Maybe (V4 Int8)
  , visible     :: Int8
  , blockData   :: V.Vector LAYRBlockData
  }
  deriving (Show)

data LAYRBlockData = LAYRBlockData
  { blockIndex :: Int32
  , blockX     :: Int32
  , blockY     :: Int32
  , blockZ     :: Int32
  }
  deriving (Show)

data Material = Material
  { materialName :: String
  , baseColor :: V4 Float
  , metallic :: Float
  , roughness :: Float
  , emission :: V3 Float
  }
  deriving (Show)

type ParseError = (ByteOffset, String)
