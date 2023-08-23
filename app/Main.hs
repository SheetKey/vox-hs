module Main where

-- app
import Parameters

-- gox-hs
import Vox.Gox
import Vox.Tree
import Vox.Tree
import Vox.Shape

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

mkGF :: Parameters -> Int -> Bool -> Double -> Double -> GoxFile
mkGF p g b x y =
  let tree = constructTree p g b
      shape = fmap TBC $ treeToTapered $ scaleFilterTree x y $ tree
  in drawShape shape emptyGoxFile

treeToFile :: Parameters -> Int -> Bool -> Double -> Double -> String -> IO ()
treeToFile p g b x y f = do
  putStrLn $ "starting: " ++ show f
  let tree = constructTree p g b
      shape = fmap TBC $ treeToTapered $ scaleFilterTree x y $ tree
      gox = drawShape shape emptyGoxFile
  writeGoxFile f gox
  putStrLn "done"

main :: IO ()
main = do
  -- treeToFile defaultP 12343 False 25 0.01 "./gen/default.gox"
  -- treeToFile acerP 12343 False 30 0.01 "./gen/acer.gox"
  -- treeToFile appleP 12343 False 15 0.1 "./gen/apple.gox"
  treeToFile balsamFirP 12343 False 15 0.1 "./gen/balsamFir.gox"
  -- treeToFile bambooP 12343 False 15 0.1 "./gen/bamboo.gox"
  -- treeToFile blackOakP 12343 False 10 0.1 "./gen/blackOak.gox"
  -- treeToFile blackTupeloP 12343 False 10 0.1 "./gen/blackTupelo.gox"
  treeToFile cambridgeOakP 12343 False 17 0.1 "./gen/cambridgeOak.gox"
  -- treeToFile douglasFirP 12343 False 10 0.1 "./gen/douglasFir.gox"
  treeToFile europeanLarchP 12343 False 15 0.1 "./gen/europeanLarch.gox"
  -- treeToFile fanPalmP 12343 False 40 0.1 "./gen/fanPalm.gox"
  treeToFile hillCherryP 12343 False 13 0.1 "./gen/hillCherry.gox"
  -- treeToFile lombardyPoplarP 12343 False 15 0.1 "./gen/lombardyPoplar.gox"
  -- treeToFile palmP 12343 False 50 0.1 "./gen/palm.gox"
  -- treeToFile quakingAspenP 12343 False 15 0.1 "./gen/quakingAspen.gox"
  treeToFile sassafrasP 12343 False 9 0.1 "./gen/sassafras.gox"
  treeToFile silverBirchP 12343 False 15 0.1 "./gen/silverBirch.gox"
  -- treeToFile smallPineP 12343 False 30 0.1 "./gen/smallPine.gox"
  -- treeToFile sphereTreeP 12343 False 30 0.1 "./gen/sphereTree.gox"
  treeToFile weepingWillowP 12343 False 15 0.1 "./gen/weepingWillow.gox"
  treeToFile weepingWillow2P 12343 False 15 0.1 "./gen/weepingWillow2.gox"
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
