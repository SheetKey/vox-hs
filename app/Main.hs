module Main where

-- vox-hs
import Vox

main :: IO ()
main = do
  let voxFile = drawCubeBottomLeft 10 10 10 0 0 0 1 emptyVoxFile
  print voxFile
  writeVoxFile "./test.vox" voxFile
  

  
--  withVoxFile "./deer.vox" $ \leftright ->
--    case leftright of
--      Left err -> print err
--      Right voxFile -> writeVoxFile "./deer2.vox" voxFile
--  withVoxFile "./deer.vox" print
