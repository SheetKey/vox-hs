module GLVoxInfo where

-- vox-hs
import qualified Gox.Type as G

-- vector
import qualified Data.Vector as V

data GLVoxInfo = GLVoxInfo
  { layers :: !(V.Vector LAYRBlockData)
  , blocks :: !(V.Vector BL16)
  }

fromGoxFile :: GoxFile -> GLVoxInfo
fromGoxFile goxFile = GLVoxInfo
  { blocks = G.blocks goxFile
  , layers = V.concatMap G.blockData (G.layers goxFile)
  }
