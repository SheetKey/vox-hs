{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Vox.Tree.Renderable where

-- base
import Data.Word
import GHC.Float (double2Float)

-- vox-hs
import Vox.Tree.Type
import Vox.Shape

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

data RCurve = RCurve
  { vertices :: VS.Vector Float -- GLfloat for vbo
  , indices :: VS.Vector Word32 -- GLunit for ebo
  }

type RStem = RCurve

data RTree = RTree
  { curves :: V.Vector RStem
  , baseRadius :: Double
  }

fromTree :: Int -> Tree -> RTree
fromTree n tree =
  let curves = fmap (fromStem n) $ tStems tree
      baseRadius = bpRadius $ (V.! 0) $ bezierPoints $ sCurve $ (V.! 0) $ tStems tree
  in RTree {..}

fromStem :: Int -> Stem -> RStem
fromStem n = fromCurve n . sCurve

ngon :: Int -> Double -> V3 Double -> V3 Double -> V3 Double -> VS.Vector Float
ngon n radius tangentVec normalVec centerVec = VS.concatMap (\ index ->
  let angle = (2 * pi) / (fromIntegral index + 1)
      rotQuat = axisAngle tangentVec angle
      pnt = rotate rotQuat $ normalVec ^* radius
      V3 x y z = double2Float <$> (centerVec + pnt)
  in VS.fromList [x, y, z])
  (VS.generate n id)

nIndices :: Int -> VS.Vector Word32
nIndices n = VS.concat
  [ VS.generate 6 $ \case
      0 -> nth
      1 -> nth + 1
      2 -> nth + fromIntegral n 
      3 -> nth + fromIntegral n 
      4 -> (nth + fromIntegral n + 1) `mod` (2 * fromIntegral n)
      5 -> (nth + 1) `mod` n
      _ -> error "not possible ('nIndices')"
  | nth <- fromIntegral <$> [0..(n-1)]
  ]

genIndices :: Int -> Int -> VS.Vector Word32
genIndices n l = VS.concatMap (\i -> VS.map (+i) (nIndices n)) (VS.generate l fromIntegral)

taperedNGon :: Double -> Int -> TaperedBezierCurve CubicBezier -> VS.Vector Float
taperedNGon t n TaperedBezierCurve {..} = ngon n (taperingFunction t)
                                          (tangent taperedBezierCurve t)
                                          (normal taperedBezierCurve t)
                                          (fst $ compute taperedBezierCurve t)

taperedStartNGon :: Int -> TaperedBezierCurve CubicBezier -> VS.Vector Float
taperedStartNGon = taperedNGon 0

taperedEndNGon :: Int -> TaperedBezierCurve CubicBezier -> VS.Vector Float
taperedEndNGon = taperedNGon 1

fromCurve :: Int -> Curve -> RCurve
fromCurve n c =
  let bc = curveToTapered c
      firstGon = taperedStartNGon n (bc V.! 0)
      vertices = V.foldl' (\ acc c -> acc VS.++ taperedEndNGon n c ) firstGon bc
      indices = genIndices n (V.length bc)
  in RCurve {..}
