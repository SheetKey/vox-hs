{-# LANGUAGE RecordWildCards #-}

module Gox.Write where

-- vox-hs
import Gox.Type

writeGoxFile :: FilePath -> GoxFile -> IO ()
writeGoxFile filePath goxFile = withBinaryFile filePath WriteMode $ \h ->
  BL.hPut h (assembleGoxFileStream goxFile)

assembleGoxFileStream :: GoxFile -> BL.ByteString
assembleGoxFileStream = runPut . putGoxFile

putGoxFile :: GoxFile -> Put
putGoxFile GoxFile {..} = do
  putString "GOX "
  putInt32le 2
  putGoxBlocks blocks
  putGoxLayers layers

putGoxBlocks :: V.Vector BL16 -> Put
putGoxBlocks = sequence_ . fmap putGoxBlock

putGoxBlock :: BL16 -> Put
putGoxBlock BL16 {..} = do
  let encodedVoxelData = encodePng voxelData
      size = fromIntegral $ BL.length encodedVoxelData
  putString "BL16"
  putInt32le size
  putLazyByteString encodedVoxelData
  putInt32le 0

putGoxLayers :: V.Vector LAYR -> Put
putGoxLayers = sequence_ . fmap putGoxLayer

putGoxLayer :: LAYR -> Put
putGoxLayer LAYR {..} = do

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)
