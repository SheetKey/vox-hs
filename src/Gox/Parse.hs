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
  parseBlocks (i-1) $ acc `V.snoc` LAYRBlockData {..}


parseLAYR :: Get GoxChunk
parseLAYR = do
  numberOfBlocks <- getInt32le
  blockData <- parseBlocks numberOfBlocks V.empty
  -- dict
  -- name
  nameKSize <- getWord32le
  _ <- if nameKSize /= 4
       then fail $ "Expected 'nameKSize == 4' but got '" ++ show nameKSize ++ "'."
       else do name <- getNChars 4
               if name /= "name"
                 then fail $ "Expected dict key 'name' but found '" ++ name ++ "'."
                 else return ()
  nameVSize <- getWord32le
  layrName <- getNChars (fromIntegral nameVSize)
  -- mat
  matKSize <- getWord32le
  _ <- if matKSize /= 3
       then fail $ "Expected 'matKSize == 3' but got '" ++ show matKSize ++ "'."
       else do name <- getNChars 3
               if name /= "mat"
                 then fail $ "Expected dict key 'mat' but found '" ++ name ++ "'."
                 else return ()
  matVSize <- getWord32le
  mat <- if fromIntegral matVSize /= (16 * sizeOf (undefined :: Float))
         then fail $ "Expected 'matVSize == " ++ show (16 * sizeOf (undefined :: Float)) ++
              "' but found '" ++ show matVSize ++ "'."
         else do a11 <- getFloatle
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
  -- id
  idKSize <- getWord32le
  _ <- if idKSize /= 2
       then fail $ "Expected 'idKSize == 2' but got '" ++ show idKSize ++ "'."
       else do name <- getNChars 2
               if name /= "id"
                 then fail $ "Expected dict key 'id' but found '" ++ name ++ "'."
                 else return ()
  idVSize <- getWord32le
  layrId <- case idVSize of
              1 -> trace "idVSize == 1" $ getInt8    >>= (return . fromIntegral)
              2 -> trace "idVSize == 2" $ getInt16le >>= (return . fromIntegral)
              4 -> trace "idVSize == 4" $ getInt32le >>= (return . fromIntegral)
              8 -> trace "idVSize == 8" $ getInt64le >>= (return . fromIntegral)
              x -> fail $ "Expected 'idVSize == 1, 2, 4, 8' but found '" ++ show x ++ "'."
  -- baseId
  _ <- parseDictKey 7 "base_id"
  baseIdVSize <- getWord32le
  baseId <- case baseIdVSize of
              1 -> trace "baseIdVSize == 1" $ getInt8    >>= (return . fromIntegral)
              2 -> trace "baseIdVSize == 2" $ getInt16le >>= (return . fromIntegral)
              4 -> trace "baseIdVSize == 4" $ getInt32le >>= (return . fromIntegral)
              8 -> trace "baseIdVSize == 8" $ getInt64le >>= (return . fromIntegral)
              x -> fail $ "Expected 'baseIdVSize == 1, 2, 4, 8' but found '" ++ show x ++ "'."
  -- material
  _ <- parseDictKey 8 "material"
  materialIdxVSize <- getWord32le
  materialIdx <- case materialIdxVSize of
                   1 -> trace "materialIdxVSize == 1" $ getInt8    >>= (return . fromIntegral)
                   2 -> trace "materialIdxVSize == 2" $ getInt16le >>= (return . fromIntegral)
                   4 -> trace "materialIdxVSize == 4" $ getInt32le >>= (return . fromIntegral)
                   8 -> trace "materialIdxVSize == 8" $ getInt64le >>= (return . fromIntegral)
                   x -> fail $ "Expected 'materialIdxVSize == 1, 2, 4, 8' but found '"
                        ++ show x ++ "'."
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
            Just _  -> do
              mBoxVSize <- getWord32le
              if fromIntegral mBoxVSize /= (16 * sizeOf (undefined :: Float))
                then fail $ "Expected 'mBoxVSize == " ++ show (16 * sizeOf (undefined :: Float)) ++
                     "' but found '" ++ show mBoxVSize ++ "'."
                else do a11 <- getFloatle
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
                        return $ Just $ V4
                          (V4 a11 a12 a13 a14)
                          (V4 a21 a22 a23 a24)
                          (V4 a31 a32 a33 a34)
                          (V4 a41 a42 a43 a44)
  -- maybe shape and color
  mShapeKey <- lookAheadM $ parseDictKeyMaybe 5 "shape"
  (mShape, mColor) <- case mShapeKey of
                        Nothing -> return (Nothing, Nothing)
                        Just _  -> do
                          shapeVSize <- getWord32le
                          shapeV <- getNChars $ fromIntegral shapeVSize
                          _ <- parseDictKey 5 "color"
                          color <- parseDictValue "color" 4 $ do
                            r <- getInt8
                            g <- getInt8
                            b <- getInt8
                            a <- getInt8
                            return $ V4 r g b a
                          return (Just shapeV, Just color)
  -- visible
  _ <- parseDictKey 7 "visible"
  visibleVSize <- getWord32le
  visible <- case visibleVSize of
               1 -> trace "visibleVSize == 1" $ getInt8    >>= (return . fromIntegral)
               2 -> trace "visibleVSize == 2" $ getInt16le >>= (return . fromIntegral)
               4 -> trace "visibleVSize == 4" $ getInt32le >>= (return . fromIntegral)
               8 -> trace "visibleVSize == 8" $ getInt64le >>= (return . fromIntegral)
               x -> fail $ "Expected 'visibleVSize == 1, 2, 4, 8' but found '" ++ show x ++ "'."
  -- return
  return $ GLAYR $ LAYR {..}

parseDictValue :: String -> Word32 -> Get a -> Get a
parseDictValue key expectedSize f = do
  size <- getWord32le
  if size /= expectedSize
    then fail $ "Expected value size '" ++ show expectedSize ++ "' but found '" ++ show size
         ++ "' for key '" ++ key ++ "'."
    else f

parseDictKey :: Word32 -> String -> Get ()
parseDictKey expectedSize expectedKey = do
  size <- getWord32le
  if size /= expectedSize
    then fail $ "Expected key size '" ++ show expectedSize ++ "' but found size '"
    ++ show size ++ "' for key '" ++ expectedKey ++ "'."
    else do key <- getNChars $ fromIntegral size
            if key /= expectedKey
              then fail $ "Expected dict key '" ++ show expectedKey ++ "' but found '"
                   ++ show key ++ "'."
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
  nameKSize <- getWord32le
  _ <- if nameKSize /= 4
       then fail $ "Expected 'nameKSize == 4' but got '" ++ show nameKSize ++ "'."
       else do name <- getNChars 4
               if name /= "name"
                 then fail $ "Expected dict key 'name' but found '" ++ name ++ "'."
                 else return ()
  nameVSize <- getWord32le
  materialName <- getNChars (fromIntegral nameVSize)
  -- baseColor
  baseColorKSize <- getWord32le
  _ <- if baseColorKSize /= 5
       then fail $ "Expected 'baseColorKSize == 5' but got '" ++ show baseColorKSize ++ "'."
       else do name <- getNChars 5
               if name /= "color"
                 then fail $ "Expected dict key 'color' but found '" ++ name ++ "'."
                 else return ()
  baseColorVSize <- getWord32le
  baseColor <- if fromIntegral baseColorVSize /= (4 * sizeOf (undefined :: Float))
    then fail $ "Expected 'baseColorVSize == " ++ show (4 * (sizeOf (undefined :: Float)))
         ++ "' but found '" ++ show (fromIntegral baseColorVSize) ++ "'."
    else do r <- getFloatle
            g <- getFloatle
            b <- getFloatle
            a <- getFloatle
            return $ V4 r g b a
  -- metallic
  metallicKSize <- getWord32le
  _ <- if metallicKSize /= 8
       then fail $ "Expected 'metallicKSize == 8' but found '" ++ show metallicKSize ++ "'."
       else do name <- getNChars 8
               if name /= "metallic"
                 then fail $ "Expected dict key 'metallic' but found '" ++ name ++ "'."
                 else return ()
  metallicVSize <- getWord32le
  metallic <- if fromIntegral metallicVSize /= sizeOf (undefined :: Float)
              then fail $ "Expected 'metallicVSize == " ++ show (sizeOf (undefined :: Float)) ++
                   "' but found '" ++ show (fromIntegral metallicVSize) ++ "'."
              else getFloatle
  -- roughness
  roughnessKSize <- getWord32le
  _ <- if roughnessKSize /= 9
       then fail $ "Expected 'roughnessKSize == 9' but found '" ++ show roughnessKSize ++ "'."
       else do name <- getNChars 9
               if name /= "roughness"
                 then fail $ "Expected dict key 'roughness' but found '" ++ name ++ "'."
                 else return ()
  roughnessVSize <- getWord32le
  roughness <- if fromIntegral roughnessVSize /= sizeOf (undefined :: Float)
               then fail $ "Expected 'roughnessVSize == " ++ show (sizeOf (undefined :: Float)) ++
                   "' but found '" ++ show (fromIntegral roughnessVSize) ++ "'."
              else getFloatle
  -- emission
  emissionKSize <- getWord32le
  _ <- if emissionKSize /= 8
       then fail $ "Expected 'emissionKSize == 8' but found '" ++ show emissionKSize ++ "'."
       else do name <- getNChars 8
               if name /= "emission"
                 then fail $ "Expected dict key 'emission' but found '" ++ name ++ "'."
                 else return ()
  emissionVSize <- getWord32le
  emission <- if fromIntegral emissionVSize /= (3 * sizeOf (undefined :: Float))
               then fail $ "Expected 'emissionVSize == "
                    ++ show (3 * sizeOf (undefined :: Float)) ++
                    "' but found '" ++ show (fromIntegral emissionVSize) ++ "'."
              else do a <- getFloatle
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




  

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier :: Get String
getIdentifier = getNChars 4
