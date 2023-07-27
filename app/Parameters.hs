module Parameters where

-- gox-hs
import Gox.Tree.Type

-- linear
import Linear

-- vector
import qualified Data.Vector as V

aspen :: Parameters
aspen = Parameters
  { pShape          = TendFlame
  , pGScale         = 13
  , pGScaleV        = 3
  , pLevels         = 3
  , pRatio          = 0.015
  , pRatioPower     = 1.2
  , pFlare          = 0.6
  , pBaseSplits     = 0
  , pBaseSize       = V.fromList [0.3, 0.02, 0.02, 0.02]
  , pDownAngle      = V.fromList [-0, 60, 45, 45]
  , pDownAngleV     = V.fromList [-0, -50, 10, 10]
  , pRotate         = V.fromList [-0, 140, 140, 77]
  , pRotateV        = V.fromList [-0, 0, 0, 0]
  , pBranches       = V.fromList [1, 50, 30, 10]
  , pLength         = V.fromList [1, 0.3, 0.6, 0]
  , pLengthV        = V.fromList [0, 0, 0, 0]
  , pTaper          = V.fromList [1, 1, 1, 1]
  , pSegSplits      = V.fromList [0, 0, 0, 0]
  , pSplitAngle     = V.fromList [40, 0, 0, 0]
  , pSplitAngleV    = V.fromList [5, 0, 0, 0]
  , pCurveRes       = V.fromList [5, 5, 3, 1]
  , pCurve          = V.fromList [0, -40, -40, 0]
  , pCurveBack      = V.fromList [0, 0, 0, 0]
  , pCurveV         = V.fromList [20, 50, 75, 0]
  , pBendV          = V.fromList [-0, 50, 0, 0]
  , pBranchDist     = V.fromList [-0, 0, 0, 0]
  , pRadiusMod      = V.fromList [1, 1, 1, 1]
  , pLeafBlosNum    = 40
  , pLeafShape      = 0
  , pLeafScale      = 0.17
  , pLeafScaleX     = 1
  , pLeafBend       = 0.6
  , pBlossomShape   = 1
  , pBlossomScale   = 0
  , pBlossomRate    = 0
  , pTropism        = V3 0 0 0.5
  , pPruneRatio     = 0
  , pPruneWidth     = 0.5
  , pPruneWidthPeak = 0.5
  , pPrunePowerLow  = 0.5
  , pPrunePowerHigh = 0.5
  }
