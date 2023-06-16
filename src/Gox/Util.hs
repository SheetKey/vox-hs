{-# LANGUAGE RecordWildCards #-}

module Gox.Util where

-- vox-hs
import Gox.Type

-- base
import Data.Int

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
