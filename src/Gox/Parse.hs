module Gox.Parse where

-- vox-hs
import Gox.Type

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

parseChunkList :: Get GoxFile
parseChunkList = do
  id <- getIdentifier
  size <- getWord32le
  (nextChunk, chunkSize) <- case id of
                              "BL16" -> f
                              "LAYR" -> g
                              "MATE" -> parseMATE

parseMATE :: Get Material
parseMATE = do
  -- name
  nameKSize <- getWord32le
  _ <- if nameKSize /= 4
       then fail $ "Expected 'nameKSize == 4' but got '" ++ show nameKSize ++ "'."
       else do name <-getNChars 4
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
  baseColor <- if baseColorVSize /= (4 * sizeOf (undefined :: Float))
    then fail $ "Expected 'baseColorVSize == " ++ show (4 * (sizeOf (undefined :: Float)))
         ++ "' but found '" ++ show baseColorVSize ++ "'."
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
  metallic <- if metallicVSize /= sizeOf (undefined :: Float)
              then fail $ "Expected 'metallicVSize == " ++ show (sizeOf (undefined :: Float)) ++
                   "' but found '" ++ show metallicVSize ++ "'."
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
  roughness <- if roughnessVSize /= sizeOf (undefined :: Float)
               then fail $ "Expected 'roughnessVSize == " ++ show (sizeOf (undefined :: Float)) ++
                   "' but found '" ++ show roughnessVSize ++ "'."
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
  emission <- if emissionVSize /= sizeOf (undefined :: Float)
               then fail $ "Expected 'emissionVSize == " ++ show (sizeOf (undefined :: Float)) ++
                   "' but found '" ++ show emissionVSize ++ "'."
              else getFloatle
  -- final key size of a dict should be '0' to signal end of dict
  zero <- getWord32le
  if zero /= 0
    then fail $ "Expected dict to end with 'keySize == 0' but found key size '"
         ++ show zero ++ "'."
    else return Material {..}





  

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier :: Get String
getIdentifier = getNChars 4
