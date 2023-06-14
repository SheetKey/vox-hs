module Main where

-- vox-hs
import Vox

main :: IO ()
main = withVoxFile "./deer.vox" print
