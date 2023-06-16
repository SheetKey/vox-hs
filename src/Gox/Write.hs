{-# LANGUAGE RecordWildCards #-}

module Gox.Write where

-- vox-hs
import Gox.Type

-- base
import System.IO (withBinaryFile, IOMode(..))
import Data.Word
import Data.Char (ord)
import Control.Monad (when)
import Foreign.Storable (sizeOf)

-- binary
import Data.Binary.Put

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- vector
import qualified Data.Vector as V

-- linear
import Linear 

-- JuicyPixels
import Codec.Picture.Png

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
  putGoxMaterials materials
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
  -- crc
  putInt32le 0 

putGoxMaterials :: V.Vector Material -> Put
putGoxMaterials = sequence_ . fmap putGoxMaterial

putGoxMaterial :: Material -> Put
putGoxMaterial Material {..} = do
  putString "MATE"
  putWord32le $
    (4 + 4 + 4 + (fromIntegral $ length materialName)) -- name
    + (4 + 5 + 4 + (fromIntegral $ 4 * sizeOf (undefined :: Float))) -- color
    + (4 + 8 + 4 + (fromIntegral $ sizeOf (undefined :: Float))) -- metallic
    + (4 + 9 + 4 + (fromIntegral $ sizeOf (undefined :: Float))) -- roughness
    + (4 + 8 + 4 + (fromIntegral $ 3 * sizeOf (undefined :: Float))) -- emission
  -- name
  putInt32le 4
  putString "name"
  putWord32le (fromIntegral $ length materialName)
  putString materialName
  -- color
  putInt32le 5
  putString "color"
  putWord32le (fromIntegral $ 4 * sizeOf (undefined :: Float))
  sequence_ $ putFloatle <$> baseColor
  -- metallic
  putInt32le 8
  putString "metallic"
  putWord32le (fromIntegral $ sizeOf (undefined :: Float))
  putFloatle metallic
  -- roughness
  putInt32le 9
  putString "roughness"
  putWord32le (fromIntegral $ sizeOf (undefined :: Float))
  putFloatle roughness
  -- emission
  putInt32le 8
  putString "emission"
  putWord32le (fromIntegral $ 3 * sizeOf (undefined :: Float))
  sequence_ $ putFloatle <$> emission
  -- crc
  putInt32le 0 
  

putGoxLayers :: V.Vector LAYR -> Put
putGoxLayers = sequence_ . fmap putGoxLayer

putGoxLayer :: LAYR -> Put
putGoxLayer LAYR {..} = do
  putString "LAYR"
  let numberOfBlocks = case (baseId, mShape) of
                         (0, Nothing) -> V.length blockData
                         _ -> 0
      imgPathSize = case mImgPath of
                      Nothing -> 0
                      Just path -> 4 + 8 + 4 + (fromIntegral $ length path)
      boxSize = case mBox of
                  Nothing -> 0
                  Just _ -> 4 + 3 + 4 + (fromIntegral $ 16 * sizeOf (undefined :: Float))
      shapeAndColorSize = case (mShape, mColor) of
                            (Just shape, Just _) -> 4 + 5 + 4 + (fromIntegral $ length shape)
                                                        + 4 + 5 + 4 + 4
                            _ -> 0
  putWord32le $
    4 -- number of blocks
    + (fromIntegral numberOfBlocks * 20) -- 20 bytes per block
    + (4 + 4 + 4 + (fromIntegral $ length layrName)) -- name
    + (4 + 3 + 4 + (fromIntegral $ 16 * sizeOf (undefined :: Float))) -- mat
    + (4 + 2 + 4 + 4) -- id
    + (4 + 7 + 4 + 4) -- base_id
    + (4 + 8 + 4 + 4) -- material
    + imgPathSize -- img-path
    + boxSize -- box
    + shapeAndColorSize -- shape and color
    + (4 + 7 + 4 + 1) -- visible
  putWord32le (fromIntegral numberOfBlocks)
  when (numberOfBlocks > 0) $ do
    putBlocks blockData
  -- name
  putDictKey 4 "name"
  putWord32le (fromIntegral $ length layrName)
  putString layrName
  -- mat
  putDictKey 3 "mat"
  putWord32le (fromIntegral $ 16 * sizeOf (undefined :: Float))
  putM44 mat
  -- id
  putDictKey 2 "id"
  putWord32le 4
  putInt32le layrId
  -- base_id
  putDictKey 7 "base_id"
  putWord32le 4
  putInt32le baseId
  -- material
  putDictKey 8 "material"
  putWord32le 4
  putInt32le materialIdx
  -- img-path
  case mImgPath of
    Nothing -> return ()
    Just path -> do putDictKey 8 "img-path"
                    putWord32le (fromIntegral $ length path)
                    putString path
  -- box
  case mBox of
    Nothing -> return ()
    Just box -> do putDictKey 3 "box"
                   putWord32le (fromIntegral $ 16 * sizeOf (undefined :: Float))
                   putM44 box
  -- shape and color
  case (mShape, mColor) of
    (Just shape, Just color) -> do
      putDictKey 5 "shape"
      putWord32le (fromIntegral $ length shape)
      putString shape
      putDictKey 5 "color"
      putWord32le 4
      sequence_ $ putInt8 <$> color
    _ -> return ()
  -- visible
  putDictKey 7 "visible"
  putWord32le 1
  putInt8 visible
  -- crc
  putInt32le 0

putM44 :: M44 Float -> Put
putM44 (V4
        (V4 a11 a12 a13 a14)
        (V4 a21 a22 a23 a24)
        (V4 a31 a32 a33 a34)
        (V4 a41 a42 a43 a44)) = do
  putFloatle a11
  putFloatle a12
  putFloatle a13
  putFloatle a14
  putFloatle a21
  putFloatle a22
  putFloatle a23
  putFloatle a24
  putFloatle a31
  putFloatle a32
  putFloatle a33
  putFloatle a34
  putFloatle a41
  putFloatle a42
  putFloatle a43
  putFloatle a44

putDictKey :: Word32 -> String -> Put
putDictKey size key = do
  putWord32le size
  putString key

putBlocks :: V.Vector LAYRBlockData -> Put
putBlocks = sequence_ . fmap putBlock

putBlock :: LAYRBlockData -> Put
putBlock LAYRBlockData {..} = do
  putInt32le blockIndex
  putInt32le blockX
  putInt32le blockY
  putInt32le blockZ
  putInt32le 0

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)
