module Vox.Type where

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
import qualified Data.Vector as V

-- containers
import Data.Map.Strict as M

type ParseError = (ByteOffset, String)

type ChunkDataSize = Word32

type ChunkSize = Word32

type ChunkId = String

type VoxFile = (Main, [VoxChunk])

type VoxString = String

type VoxDict = M.Map VoxString VoxString

data Main = Main { totalSize :: Word32 } deriving (Show)

data Pack = Pack { numModels :: Int32 } deriving (Show)

data Size = Size { x :: Int32, y :: Int32, z :: Int32 } deriving (Show)-- z is in the direction of gravity (Not oriented as in opengl)

data XYZI = XYZI { numVoxels :: Int32, voxels :: U.Vector (Int8, Int8, Int8, Int8) } deriving (Show)

data RGBA = RGBA { rgba :: U.Vector (Int8, Int8, Int8, Int8) } deriving (Show)

data NTRN = NTRN { nTRNId :: Int32
                 , nTRNAttr :: VoxDict
                 , nTRNChildId :: Int32
                 , nTRNReserved :: Int32
                 , nTRNLayerId :: Int32
                 , nTRNNumFrames :: Int32
                 , nTRNFrameAttrs :: V.Vector VoxDict
                 , nTRNSize :: ChunkDataSize
                 }
  deriving (Show)

data VoxChunk = VPack Pack
              | VModel Size XYZI
              | VRGBA RGBA
              | VnTRN NTRN
  deriving (Show)
