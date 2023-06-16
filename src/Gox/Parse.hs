{-# LANGUAGE RecordWildCards #-}

module Gox.Parse where

-- vox-hs
import Gox.Type

-- base
import System.IO (withBinaryFile, IOMode(..))
import Data.Word
import Data.Int
import Data.Char (chr)
import Control.Monad (replicateM)
import Foreign.Storable (sizeOf)
import Debug.Trace (trace)

-- binary
import Data.Binary.Get

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- containers
import Data.Map.Strict as M

-- linear
import Linear hiding (trace)

-- JuicyPixels
import Codec.Picture.Types
import Codec.Picture.Png

withGoxFile :: FilePath -> (Either ParseError GoxFile -> IO ()) -> IO ()
withGoxFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
  goxData <- fmap parseGoxFileStream (BL.hGetContents h)
  action goxData

parseGoxFileStream :: BL.ByteString -> Either ParseError GoxFile
parseGoxFileStream input =
  case runGetOrFail getGoxFile input of
    Left (_, offset, error) -> Left (offset, error)
    Right (_, _, result) -> Right result

getGoxFile :: Get GoxFile
getGoxFile = do
  id <- getIdentifier
  if id /= "GOX "
    then fail $ "GOX file not allowed to start with '" ++ id ++ "'. Must start with 'GOX '."
    else do
    version <- getInt32le
    if version /= 2
      then fail $ "GOX file has verion '" ++ show version ++ "'. Must have version '2'."
      else do
      parseChunks 

parseChunks :: Get GoxFile
parseChunks = do
  dontContinue <- isEmpty
  if dontContinue
    then return $ fromChunk None
    else do chunk <- parseChunk
            following <- parseChunks
            return $ addChunk chunk following

parseChunk :: Get GoxChunk
parseChunk = do
  id <- getIdentifier
  size <- getWord32le
  chunk <- case id of
             "BL16" -> parseBL16 size
             "LAYR" -> parseLAYR
             "MATE" -> parseMATE
             _ -> do skip $ fromIntegral size
                     return Skipped
  _ <- getWord32le -- crc
  return chunk

parseBlocks :: Int32 -> V.Vector LAYRBlockData -> Get (V.Vector LAYRBlockData)
parseBlocks 0 acc = return acc
parseBlocks i acc = do
  blockIndex <- getInt32le
  blockX     <- getInt32le
  blockY     <- getInt32le
  blockZ     <- getInt32le
  _ <- getInt32le -- should be 0
  parseBlocks (i-1) $ LAYRBlockData {..} `V.cons` acc


parseLAYR :: Get GoxChunk
parseLAYR = do
  numberOfBlocks <- getInt32le
  blockData <- parseBlocks numberOfBlocks V.empty
  -- dict
  -- name
  _ <- parseDictKey 4 "name" "LAYR"
  nameVSize <- getWord32le
  layrName <- getNChars (fromIntegral nameVSize)
  -- mat
  _ <- parseDictKey 3 "mat" "LAYR"
  mat <- parseDictValue "mat" (fromIntegral $ 16 * sizeOf (undefined :: Float)) parseM44
  -- id
  -- it seems 'id' uses Int32, not documented
  _ <- parseDictKey 2 "id" "LAYR"
  layrId <- parseDictValue "id" 4 getInt32le 
  -- baseId
  -- it seems 'base_id' uses Int32, not documented
  _ <- parseDictKey 7 "base_id" "LAYR"
  baseId <- parseDictValue "base_id" 4 getInt32le 
  -- material
  -- it seems 'material uses Int32, not documented
  _ <- parseDictKey 8 "material" "LAYR"
  materialIdx <- parseDictValue "material" 4 getInt32le 
  -- mayebe img-path
  mImagePathKey <- lookAheadM $ parseDictKeyMaybe 8 "img-path"
  mImgPath <- case mImagePathKey of
                Nothing -> return Nothing
                Just _  -> do size <- getWord32le
                              imgPath <- getNChars $ fromIntegral size
                              return $ Just imgPath
  -- maybe box
  mBoxKey <- lookAheadM $ parseDictKeyMaybe 3 "box"
  mBox <- case mBoxKey of
            Nothing -> return Nothing
            Just _  -> parseDictValue "box" (fromIntegral $ 16 * sizeOf (undefined :: Float))
                       (parseM44 >>= (return . Just))
  -- maybe shape and color
  mShapeKey <- lookAheadM $ parseDictKeyMaybe 5 "shape"
  (mShape, mColor) <- case mShapeKey of
                        Nothing -> return (Nothing, Nothing)
                        Just _  -> do
                          shapeVSize <- getWord32le
                          shapeV <- getNChars $ fromIntegral shapeVSize
                          _ <- parseDictKey 5 "color" "LAYR"
                          color <- parseDictValue "color" 4 $ do
                            r <- getInt8
                            g <- getInt8
                            b <- getInt8
                            a <- getInt8
                            return $ V4 r g b a
                          return (Just shapeV, Just color)
  -- visible
  -- it seems 'visible' uses Int8, not documented
  _ <- parseDictKey 7 "visible" "LAYR"
  visible <- parseDictValue "visible" 1 getInt8
  -- return
  return $ GLAYR $ LAYR {..}

parseBL16 :: ChunkSize -> Get GoxChunk
parseBL16 size = do
  png <- getByteString $ fromIntegral size
  image <- case decodePng png of
             Left err -> fail $ err
             Right dynamicImage -> case dynamicImage of
                                     ImageRGBA8 image -> return image
                                     _ -> fail $ "Failed to decode BL16 as 'RGBA8' png."
  case (imageWidth image, imageHeight image) of
    (64, 64) -> return $ GBL16 $ BL16 image
    (w , h ) -> fail $ "Expected png width and height of 64 but found w: "
                ++ show w ++ ", h: " ++ show h ++ "."

parseMATE :: Get GoxChunk
parseMATE = do
  -- name
  _ <- parseDictKey 4 "name" "MATE"
  nameVSize <- getWord32le
  materialName <- getNChars (fromIntegral nameVSize)
  -- baseColor
  _ <- parseDictKey 5 "color" "MATE"
  baseColor <- parseDictValue "color" (fromIntegral $ 4 * sizeOf (undefined :: Float)) $ do
    r <- getFloatle
    g <- getFloatle
    b <- getFloatle
    a <- getFloatle
    return $ V4 r g b a
  -- metallic
  _ <- parseDictKey 8 "metallic" "MATE"
  metallic <- parseDictValue "metallic" (fromIntegral $ sizeOf (undefined :: Float)) getFloatle
  -- roughness
  _ <- parseDictKey 9 "roughness" "MATE"
  roughness <- parseDictValue "roughness" (fromIntegral $ sizeOf (undefined :: Float)) getFloatle
  -- emission
  _ <- parseDictKey 8 "emission" "MATE"
  emission <- parseDictValue "emission" (fromIntegral $ 3 * sizeOf (undefined :: Float)) $ do
    a <- getFloatle
    b <- getFloatle
    c <- getFloatle
    return $ V3 a b c
  -- This is in the specs but not the implementation.
  -- -- final key size of a dict should be '0' to signal end of dict
  -- zero <- getWord32le
  -- if zero /= 0
  --   then fail $ "Expected dict to end with 'keySize == 0' but found key size '"
  --        ++ show zero ++ "'."
  --   else return $ GMaterial $ Material {..}
  return $ GMaterial $ Material {..}

parseDictValue :: String -> Word32 -> Get a -> Get a
parseDictValue key expectedSize f = do
  size <- getWord32le
  if size /= expectedSize
    then fail $ "Expected value size '" ++ show expectedSize ++ "' but found '" ++ show size
         ++ "' for key '" ++ key ++ "'."
    else f

parseDictKey :: Word32 -> String -> String -> Get ()
parseDictKey expectedSize expectedKey belongsTo = do
  size <- getWord32le
  if size /= expectedSize
    then fail $ "Expected key size '" ++ show expectedSize ++ "' but found size '"
    ++ show size ++ "' for key '" ++ expectedKey ++ "' in chunk '" ++ belongsTo ++ "'."
    else do key <- getNChars $ fromIntegral size
            if key /= expectedKey
              then fail $ "Expected dict key '" ++ show expectedKey ++ "' but found '"
                   ++ show key ++ "' in chunk '" ++ show belongsTo ++ "'."
              else return ()

parseDictKeyMaybe :: Word32 -> String -> Get (Maybe ())
parseDictKeyMaybe expectedSize expectedKey = do
  size <- getWord32le
  if size /= expectedSize
    then return Nothing
    else do key <- getNChars $ fromIntegral size
            if key /= expectedKey
              then return Nothing
              else return $ Just ()

parseM44 :: Get (M44 Float)
parseM44 = do 
    a11 <- getFloatle
    a12 <- getFloatle
    a13 <- getFloatle
    a14 <- getFloatle
    a21 <- getFloatle
    a22 <- getFloatle
    a23 <- getFloatle
    a24 <- getFloatle
    a31 <- getFloatle
    a32 <- getFloatle
    a33 <- getFloatle
    a34 <- getFloatle
    a41 <- getFloatle
    a42 <- getFloatle
    a43 <- getFloatle
    a44 <- getFloatle
    return $ V4
      (V4 a11 a12 a13 a14)
      (V4 a21 a22 a23 a24)
      (V4 a31 a32 a33 a34)
      (V4 a41 a42 a43 a44)

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier :: Get String
getIdentifier = getNChars 4
