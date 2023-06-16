{-# LANGUAGE RecordWildCards #-}

module Gox.Util where

-- vox-hs
import Gox.Type

-- base
import Data.Int

-- vector
import qualified Data.Vector as V

-- linear
import Linear

fromChunk :: GoxChunk -> GoxFile
fromChunk (GBL16 c) = GoxFile (V.singleton c) V.empty V.empty
fromChunk (GLAYR c) = GoxFile V.empty (V.singleton c) V.empty
fromChunk (GMaterial c) = GoxFile V.empty V.empty (V.singleton c)
fromChunk Skipped = GoxFile V.empty V.empty V.empty
fromChunk None = GoxFile V.empty V.empty V.empty

addChunk :: GoxChunk -> GoxFile -> GoxFile
addChunk (GBL16 c) GoxFile {..} = GoxFile { blocks = c `V.cons` blocks, .. }
addChunk (GLAYR c) GoxFile {..} = GoxFile { layers = c `V.cons` layers, .. }
addChunk (GMaterial c) GoxFile {..} = GoxFile { materials = c `V.cons` materials, .. }
addChunk Skipped goxFile = goxFile
addChunk None goxFile = goxFile

-- matches file created by 'File->New' in the goxel editor
defaultGoxFile :: GoxFile
defaultGoxFile = GoxFile
  { blocks = V.empty
  , layers = V.singleton layer1
  , materials = V.singleton defaultMaterial
  }
  where
    layer1 = LAYR
      { layrName    = "Layer.1"
      , mat         = identity
      , layrId      = 2
      , baseId      = 0
      , materialIdx = 0
      , mImgPath    = Nothing
      , mBox        = Nothing
      , mShape      = Nothing
      , mColor      = Nothing
      , visible     = 1
      , blockData   = V.empty
      }
    defaultMaterial = Material
      { materialName = "Material.1"
      , baseColor    = V4 1 1 1 1
      , metallic     = 0.2
      , roughness    = 0.5
      , emission     = V3 0 0 0
      }

emptyGoxFile :: GoxFile
emptyGoxFile = GoxFile
  { blocks = V.empty
  , layers = V.empty
  , materials = V.empty
  }

addMaterial
  :: Maybe String -- ^ optionally provide a name
  -> V4 Float     -- ^ base color
  -> Float        -- ^ metallic
  -> Float        -- ^ roughness
  -> V3 Float     -- ^ emission
  -> GoxFile
  -> GoxFile
addMaterial mName baseColor metallic roughness emission GoxFile {..} = GoxFile
  { materials = V.snoc materials newMaterial
  , ..
  }
  where
    newMaterial = case mName of
      Just materialName -> Material {..}
      Nothing -> Material { materialName = "Material." ++ show (V.length materials + 1), .. }

shiftGoxLayers :: Int32 -> Int32 -> Int32 -> GoxFile -> GoxFile
shiftGoxLayers x y z GoxFile {..} = GoxFile
  { layers = shiftLAYRLayers x y z <$> layers
  , ..
  }

shiftLAYRLayers :: Int32 -> Int32 -> Int32 -> LAYR -> LAYR
shiftLAYRLayers x y z LAYR {..} = LAYR
  { blockData = shiftLAYRBlock x y z <$> blockData
  , ..
  }

shiftLAYRBlock :: Int32 -> Int32 -> Int32 -> LAYRBlockData -> LAYRBlockData
shiftLAYRBlock x y z LAYRBlockData {..} = LAYRBlockData
  { blockX = blockX + x
  , blockY = blockY + y
  , blockZ = blockZ + z
  , ..
  }
