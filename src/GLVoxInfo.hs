module GLVoxInfo where

-- vox-hs
import qualified Gox.Type as G

-- base
import Data.Word

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- JuicyPixels
import Codec.Picture.Types

data GLVoxInfo = GLVoxInfo
  { layers :: !(V.Vector LBD)
  , blocks :: !(V.Vector (VS.Vector Float))
  }

fromGoxFile :: G.GoxFile -> GLVoxInfo
fromGoxFile goxFile = GLVoxInfo
  { blocks = ((VS.map fromIntegral) . imageData . G.voxelData) <$> G.blocks goxFile
  , layers = V.concatMap (fmap fromLAYRBlockData . G.blockData) (G.layers goxFile)
  }

data LBD = LBD
  { blockIndex :: Int
  , blockX     :: Float
  , blockY     :: Float
  , blockZ     :: Float
  }

fromLAYRBlockData :: G.LAYRBlockData -> LBD
fromLAYRBlockData lbd = LBD
  { blockIndex = fromIntegral (G.blockIndex lbd)
  , blockX = fromIntegral (G.blockX lbd)
  , blockY = fromIntegral (G.blockY lbd)
  , blockZ = fromIntegral (G.blockZ lbd)
  }
