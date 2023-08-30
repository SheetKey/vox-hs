{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Vox.Tree.Renderable where

-- base
import Data.Word
import GHC.Float (double2Float)
import Debug.Trace (trace)

-- vox-hs
import Vox.Tree.Type
import Vox.Shape

-- linear
import Linear hiding (trace)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

data RCurve = RCurve
  { vertices :: VS.Vector Float -- GLfloat for vbo
  , indices :: VS.Vector Word32 -- GLunit for ebo
  }
  deriving (Show)

type RStem = RCurve

data RTree = RTree
  { curves :: V.Vector RStem
  , baseRadius :: Double
  }
  deriving (Show)

fromTree :: Int -> Tree -> RTree
fromTree n tree =
  let curves = fmap (fromStem n) $ tStems tree
      baseRadius = bpRadius $ (V.! 0) $ bezierPoints $ sCurve $ (V.! 0) $ tStems tree
  in RTree {..}

fromStem :: Int -> Stem -> RStem
fromStem n = fromCurve n . sCurve

ngon :: Int -> Double -> V3 Double -> V3 Double -> V3 Double -> VS.Vector Float
ngon n radius tangentVec normalVec centerVec = VS.concatMap (\ index ->
  let angle = (2 * pi) * (fromIntegral index + 1) / (fromIntegral n)
      rotQuat = axisAngle tangentVec angle
      pnt = rotate rotQuat $ normalVec ^* radius
      V3 x y z = double2Float <$> (centerVec + pnt)
  in if pnt == (V3 0 0 0) then trace "pnt==0" $
                               VS.fromList [x, y, z]
  else VS.fromList [x, y, z])
  (VS.generate n id)

nIndices :: Int -> VS.Vector Word32
nIndices n = VS.concat
  [ VS.generate 6 $ \case
      0 -> fromIntegral $ nth
      1 -> fromIntegral $ (nth + 1) `mod` n
      2 -> fromIntegral $ nth + n 
      3 -> fromIntegral $ nth + n 
      4 -> fromIntegral $ n + ((nth + 1) `mod` n) 
      5 -> fromIntegral $ (nth + 1) `mod` n
      _ -> error "not possible ('nIndices')"
  | nth <- fromIntegral <$> [0..(n-1)]
  ]

genIndices :: Int -> Int -> VS.Vector Word32
genIndices n l = VS.concatMap
                 (\i -> VS.map (+ (i * fromIntegral n)) (nIndices n))
                 (VS.generate l fromIntegral)

taperedNGon :: Double -> Int -> TaperedBezierCurve CubicBezier -> VS.Vector Float
taperedNGon t n TaperedBezierCurve {..} = ngon n (taperingFunction t)
                                          (tangent taperedBezierCurve t)
                                          (normal taperedBezierCurve t)
                                          (fst $ compute taperedBezierCurve t)

fromCurve :: Int -> Curve -> RCurve
fromCurve n c =
  let bc = curveToTapered c
      vertices = V.foldl'
        (\ acc c -> acc VS.++ (VS.concat [ taperedNGon 0 n c
                                         , taperedNGon 0.5 n c
                                         , taperedNGon 1 n c ]
                              )) VS.empty bc
      indices = genIndices n ((3 * V.length bc) - 1)
  in RCurve {..}
