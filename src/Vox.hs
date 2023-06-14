{-# LANGUAGE RecordWildCards #-}

module Vox where

-- base
import System.IO (withBinaryFile, IOMode(..))
import Data.Word
import Data.Int
import Data.Char (chr)
import Control.Monad (replicateM)

-- binary
import Data.Binary.Get

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- vector
import qualified Data.Vector.Unboxed as U

type ParseError = (ByteOffset, String)

type ChunkDataSize = Word32

type ChunkId = String

type VoxFile = [VoxChunk]

withVoxFile :: FilePath -> (Either ParseError VoxFile -> IO ()) -> IO ()
withVoxFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
  voxData <- fmap parseVoxFileStream (BL.hGetContents h)
  action voxData

parseVoxFileStream :: BL.ByteString -> Either ParseError VoxFile
parseVoxFileStream input =
  case runGetOrFail getVoxFile input of
    Left (_, offset, error) -> Left (offset, error)
    Right (_, _, result) -> Right result

getVoxFile :: Get VoxFile
getVoxFile = do
  id <- getIdentifier
  if id /= "VOX "
    then fail $ "VOX file not allowed to start with '" ++ id ++ "'. Must start with 'VOX '."
    else do
    version <- getInt32le
    if version /= 150
      then fail $ "VOX file has verion '" ++ show version ++ "'. Must have version '150'."
      else do
      parseChunkMain

parseChunkMain :: Get [VoxChunk]
parseChunkMain = do
  id <- getIdentifier
  if id /= "MAIN"
    then fail $ "The root chunk of the VOX file is '" ++ id ++ "'. Must have root chunk 'MAIN'."
    else do
    size <- getWord32le
    childSize <- getWord32le
    if size /= 0
      then fail $ "The 'MAIN' chunk of the VOX file has content size '" ++ show size ++ "'. Expected '0'."
      else parseChunkList childSize -- need to prepend 'MAIN' chunk to the list

parseChunkList :: ChunkDataSize -> Get [VoxChunk]
parseChunkList totalSize = do
  id <- getIdentifier
  size <- getWord32le
  childSize <- getWord32le
  if childSize /= 0
    then fail $ "Chunk with id '" ++ id ++ "' has 'childSize' '" ++ show childSize ++ "'. Expected '0'."
    else do
    nextChunk <- case id of
                   "PACK" -> parsePack size
                   "SIZE" -> parseSize size 
                   "XYZI" -> parseXYZI size
                   "RGBA" -> parseRGBA size
                   --"nTRN" -> notImplemented size
                   --"nGRP" -> notImplemented size
                   --"nSHP" -> notImplemented size
                   --"MATL" -> notImplemented size
                   --"LAYR" -> notImplemented size
                   --"rOBJ" -> notImplemented size
                   _      -> fail $ "Chunk has id '" ++ id ++ "'."
    let chunkSize = 12 + size
    if totalSize <= chunkSize
      then return [nextChunk]
      else do
      following <- parseChunkList (totalSize - chunkSize)
      return $ nextChunk : following

data Main = Main { totalSize :: Int32 } deriving (Show)
data Pack = Pack { numModels :: Int32 } deriving (Show)
data Size = Size { x :: Int32, y :: Int32, z :: Int32 } deriving (Show)-- z is in the direction of gravity (Not oriented as in opengl)
data XYZI = XYZI { numVoxels :: Int32, voxels :: U.Vector (Int8, Int8, Int8, Int8) } deriving (Show)
data RGBA = RGBA { rgba :: U.Vector (Int8, Int8, Int8, Int8) } deriving (Show)
data VoxChunk = VMain Main
              | VPack Pack
              | VSize Size
              | VXYZI XYZI
              | VRGBA RGBA
  deriving (Show)

parsePack :: ChunkDataSize -> Get VoxChunk
parsePack packSize =
  if packSize /= 4
  then fail $ "'PACK' chunk has size '" ++ show packSize ++ "'. Expected 4."
  else do
    numModels <- getInt32le
    return $ VPack $ Pack numModels

parseSize :: ChunkDataSize -> Get VoxChunk
parseSize sizeSize =
  if sizeSize /= 12
  then fail $ "'SIZE' chunk has size '" ++ show sizeSize ++ "'. Expected '4 + 4 + 4'."
  else do
    x <- getInt32le
    y <- getInt32le
    z <- getInt32le
    return $ VSize $ Size {..}

parseXYZI :: ChunkDataSize -> Get VoxChunk
parseXYZI xyziSize =
  if (xyziSize - 4) `mod` 4 /= 0
  then fail $ "'XYZI' chunk has size '" ++ show xyziSize ++ "'. Expected '(xyziSize - 4) `mod` 4 == 0'."
  else do
    numVoxels <- getInt32le
    voxels <- go numVoxels U.empty
    return $ VXYZI $ XYZI {..}
  where 
    go 0 acc = return acc
    go i acc = do
      x <- getInt8
      y <- getInt8
      z <- getInt8
      c <- getInt8
      go (i-1) $ acc `U.snoc` (x, y, z, c)

parseRGBA :: ChunkDataSize -> Get VoxChunk
parseRGBA rgbaSize =
  if rgbaSize /= 4 * 256
  then fail $ "'RGBA' chunk has size '" ++ show rgbaSize ++ "'. Expected '4 * 256'."
  else do
    rgba <- go 256 U.empty
    return $ VRGBA $ RGBA {..}
  where 
    go 0 acc = return acc
    go i acc = do
      r <- getInt8
      g <- getInt8
      b <- getInt8
      a <- getInt8
      go (i-1) $ acc `U.snoc` (r, g, b, a)

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier :: Get String
getIdentifier = getNChars 4
