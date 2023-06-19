module Main where

-- gox-hs
import Gox

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- linear
import Linear

-- JuicyPixels
import Codec.Picture.Types

-- pretty-simple
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  let shape = (BezierCurve $ CubicBezier 0 0 0 100 80 0 0 100 0 50 0 0 3)
      goxFile = drawShape shape emptyGoxFileMaterial
  print $ compute (CubicBezier 0 0 0 100 80 0 0 100 0 50 0 0 3) 0.53
  putStrLn " "
  print $ bezieraabb (CubicBezier 0 0 0 100 80 0 0 100 0 50 0 0 3)
  putStrLn " "
  print $ getaabb shape
  putStrLn " "
  pPrint $ emptyBL16 shape
  putStrLn " "
  print goxFile
  putStrLn " "
  writeGoxFile "./test.gox" goxFile 
  withGoxFile "./test.gox" print

  -- writeGoxFile "./test.gox" $ addMaterial Nothing (V4 10 0 0 1) 0.5 0.3 (V3 0 0 0) defaultGoxFile
  -- withGoxFile "./deer.gox" $ \leftright ->
  --   case leftright of
  --     Left err -> print err
  --     Right goxFile -> do
  --       print goxFile
  --       writeGoxFile "./deer2.gox" $ shiftGoxLayers 0 0 130 goxFile
  -- putStrLn " "
  -- withGoxFile "./deer2.gox" print
