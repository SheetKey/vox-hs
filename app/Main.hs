module Main where

-- vox-hs
import Vox

main :: IO ()
main = withVoxFile "./deer.vox" $ \leftright ->
  case leftright of
    Left err -> print err
    Right voxFile -> writeVoxFile "./deer2.vox" voxFile
