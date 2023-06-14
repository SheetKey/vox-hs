{-# LANGUAGE RecordWildCards #-}

module Vox.Write where

-- vox-hs
import Vox.Type

-- base
import System.IO (withBinaryFile, IOMode(..))
import Data.Char (ord)

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- binary
import Data.Binary.Put

-- containers
import qualified Data.Map.Strict as M

-- vector
import qualified Data.Vector.Unboxed as U

writeVoxFile :: FilePath -> VoxFile -> IO ()
writeVoxFile filePath voxFile = withBinaryFile filePath WriteMode $ \h ->
  BL.hPut h (assembleVoxFileStream voxFile)

assembleVoxFileStream :: VoxFile -> BL.ByteString
assembleVoxFileStream = runPut . putVoxFile

putVoxFile :: VoxFile -> Put
putVoxFile (Main size, voxChunks) = do
  putString "VOX "
  putInt32le 150
  putString "MAIN"
  putWord32le 0
  putWord32le size
  sequence_ $ fmap putVoxChunk voxChunks

putVoxChunk :: VoxChunk -> Put
putVoxChunk (VPack Pack {..}) = do
  putString "PACK"
  putInt32le 4
  putInt32le 0
  putInt32le numModels
putVoxChunk (VModel Size {..} XYZI {..}) = do
  putString "SIZE"
  putInt32le 12
  putInt32le 0
  putInt32le x
  putInt32le y
  putInt32le z
  putString "XYZI"
  putInt32le $ 4 + (4 * numVoxels)
  putInt32le 0
  putInt32le numVoxels
  U.mapM_ (\(x, y, z, c) -> do
              putInt8 x
              putInt8 y
              putInt8 z
              putInt8 c
          ) voxels
putVoxChunk (VRGBA RGBA {..}) = do
  putString "RGBA"
  putInt32le $ 4 * 256
  putInt32le 0
  U.mapM_ (\(r, g, b, a) -> do
              putInt8 r
              putInt8 g
              putInt8 b
              putInt8 a
          ) rgba
putVoxChunk (VnTRN NTRN {..}) = do
  putString "nTRN"
  putWord32le nTRNSize
  putInt32le 0
  putInt32le nTRNId
  putVoxDict nTRNAttr
  putInt32le nTRNChildId
  putInt32le (-1)
  putInt32le nTRNLayerId
  putInt32le nTRNNumFrames
  sequence_ $ fmap putVoxDict nTRNFrameAttrs

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)

putVoxDict :: VoxDict -> Put
putVoxDict dict = do
  putInt32le $ fromIntegral $ M.size dict
  sequence_ $ fmap (\(k, v) -> do
                       putString k
                       putString v
                   ) (M.toList dict)
