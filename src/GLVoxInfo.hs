module GLVoxInfo where

-- vox-hs
import qualified Gox.Type as G

-- vector
import qualified Data.Vector as V

data GLVoxInfo = GLVoxInfo
  { layers :: !(V.Vector G.LAYRBlockData)
  , blocks :: !(V.Vector G.BL16)
  }

fromGoxFile :: G.GoxFile -> GLVoxInfo
fromGoxFile goxFile = GLVoxInfo
  { blocks = G.blocks goxFile
  , layers = V.concatMap G.blockData (G.layers goxFile)
  }
