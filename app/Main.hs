module Main where

-- app
import Parameters

-- gox-hs
import Gox
import Gox.Tree.Type
import Gox.Tree.Algorithm
import Gox.Shape.Bezier

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- hmatrix
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

-- linear
import Linear

-- JuicyPixels
import Codec.Picture.Types

-- pretty-simple
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  --let p0 = V3 0 0 0
  --    p1 = V3 (1/3) 0 0
  --    p2 = V3 (2/3) (1/3) 0
  --    p3 = V3 1 1 1
  --    mat = mkS2 p0 p1 p2 p3
  --    (_, s, v) = svd mat
  --    mat1 = mkS1 p0 p1 p2 p3
  --    (_, s1, v1) = svd mat1
  --print mat
  --putStrLn " "
  --print $ nullspace mat
  --putStrLn " "
  --print s
  --putStrLn " "
  --print v
  --putStrLn " "
  --print mat1
  --putStrLn " "
  --print $ nullspace mat1
  --putStrLn " "
  --print s1
  --putStrLn " "
  --print v1
  --putStrLn " "
  --print $ nullS1toM (nullspace mat1)
  --putStrLn " "
  --print $ deltaS1 (nullS1toM (nullspace mat1)) (V3 0 0 0)
  --print $ deltaS1 (nullS1toM (nullspace mat1)) (V3 1 0 0)
  --print $ deltaS1 (nullS1toM (nullspace mat1)) (V3 1 1 0)
  --print $ deltaS1 (nullS1toM (nullspace mat1)) (V3 1 1 1)

  
  let tree = constructTree aspen 12345 False
      shape = fmap TBC $ treeToTapered $ scaleFilterTree 200 0.5 $ tree
      --shape = TBC $ TaperedBezierCurve { taperedBezierCurve = CubicBezier
      --                                                        { cx0 = 0
      --                                                        , cy0 = 0
      --                                                        , cz0 = 0
      --                                                        , cx1 = 0
      --                                                        , cy1 = 50
      --                                                        , cz1 = 50
      --                                                        , cx2 = 50
      --                                                        , cy2 = 100
      --                                                        , cz2 = 50
      --                                                        , cx3 = 150
      --                                                        , cy3 = 0
      --                                                        , cz3 = 150
      --                                                        }
      --                                 , taperingFunction = const 5
      --                                 , taperMaxRadius = 2
      --                                 }
      goxFile = drawShape shape emptyGoxFile
  print $ V.length shape
  --pPrint shape
  --putStrLn " "
  --print goxFile
  -- putStrLn " "
  writeGoxFile "./test.gox" goxFile 
  --withGoxFile "./test.gox" print

  -- writeGoxFile "./test.gox" $ addMaterial Nothing (V4 10 0 0 1) 0.5 0.3 (V3 0 0 0) defaultGoxFile
  -- withGoxFile "./deer.gox" $ \leftright ->
  --   case leftright of
  --     Left err -> print err
  --     Right goxFile -> do
  --       print goxFile
  --       writeGoxFile "./deer2.gox" $ shiftGoxLayers 0 0 130 goxFile
  -- putStrLn " "
  -- withGoxFile "./deer2.gox" print
