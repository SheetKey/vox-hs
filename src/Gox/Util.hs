{-# LANGUAGE RecordWildCards #-}

module Gox.Util where

-- vox-hs
import Gox.Type

-- base
import Data.Int
import Data.Word
import Data.Tuple (swap)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- JuicyPixels
import Codec.Picture.Types

-- linear
import Linear

fromChunk :: GoxChunk -> GoxFile
fromChunk (GBL16 c) = GoxFile (V.singleton c) V.empty V.empty
fromChunk (GLAYR c) = GoxFile V.empty (V.singleton c) V.empty
fromChunk (GMaterial c) = GoxFile V.empty V.empty (V.singleton c)
fromChunk Skipped = GoxFile V.empty V.empty V.empty
fromChunk None = GoxFile V.empty V.empty V.empty

addChunk :: GoxChunk -> GoxFile -> GoxFile
addChunk (GBL16 c) GoxFile {..} = GoxFile { blocks = c `V.cons` blocks, .. }
addChunk (GLAYR c) GoxFile {..} = GoxFile { layers = c `V.cons` layers, .. }
addChunk (GMaterial c) GoxFile {..} = GoxFile { materials = c `V.cons` materials, .. }
addChunk Skipped goxFile = goxFile
addChunk None goxFile = goxFile

-- matches file created by 'File->New' in the goxel editor
defaultGoxFile :: GoxFile
defaultGoxFile = GoxFile
  { blocks = V.empty
  , layers = V.singleton layer1
  , materials = V.singleton defaultMaterial
  }
  where
    layer1 = LAYR
      { layrName    = "Layer.1"
      , mat         = identity
      , layrId      = 2
      , baseId      = 0
      , materialIdx = 0
      , mImgPath    = Nothing
      , mBox        = Nothing
      , mShape      = Nothing
      , mColor      = Nothing
      , visible     = 1
      , blockData   = V.empty
      }
    defaultMaterial = Material
      { materialName = "Material.1"
      , baseColor    = V4 1 1 1 1
      , metallic     = 0.2
      , roughness    = 0.5
      , emission     = V3 0 0 0
      }

emptyGoxFile :: GoxFile
emptyGoxFile = GoxFile
  { blocks = V.empty
  , layers = V.empty
  , materials = V.empty
  }

emptyGoxFileMaterial :: GoxFile
emptyGoxFileMaterial = GoxFile
  { blocks = V.empty
  , layers = V.empty
  , materials = V.singleton defaultMaterial
  }
  where
    defaultMaterial = Material
      { materialName = "Material.1"
      , baseColor    = V4 1 1 1 1
      , metallic     = 0.2
      , roughness    = 0.5
      , emission     = V3 0 0 0
      }

addMaterial
  :: Maybe String -- ^ optionally provide a name
  -> V4 Float     -- ^ base color
  -> Float        -- ^ metallic
  -> Float        -- ^ roughness
  -> V3 Float     -- ^ emission
  -> GoxFile
  -> GoxFile
addMaterial mName baseColor metallic roughness emission GoxFile {..} = GoxFile
  { materials = V.snoc materials newMaterial
  , ..
  }
  where
    newMaterial = case mName of
      Just materialName -> Material {..}
      Nothing -> Material { materialName = "Material." ++ show (V.length materials + 1), .. }

shiftGoxLayers :: Int32 -> Int32 -> Int32 -> GoxFile -> GoxFile
shiftGoxLayers x y z GoxFile {..} = GoxFile
  { layers = shiftLAYRLayers x y z <$> layers
  , ..
  }

shiftLAYRLayers :: Int32 -> Int32 -> Int32 -> LAYR -> LAYR
shiftLAYRLayers x y z LAYR {..} = LAYR
  { blockData = shiftLAYRBlock x y z <$> blockData
  , ..
  }

shiftLAYRBlock :: Int32 -> Int32 -> Int32 -> LAYRBlockData -> LAYRBlockData
shiftLAYRBlock x y z LAYRBlockData {..} = LAYRBlockData
  { blockX = blockX + x
  , blockY = blockY + y
  , blockZ = blockZ + z
  , ..
  }

newLAYR :: V.Vector LAYRBlockData -> Int -> LAYR
newLAYR blockData l = LAYR 
  { layrName = "Layer." ++ show l
  , mat = identity
  , layrId = fromIntegral l
  , baseId = 0
  , materialIdx = 0
  , mImgPath = Nothing
  , mBox = Nothing
  , mShape = Nothing
  , mColor = Nothing
  , visible = 1
  , ..
  }

preBL16ToBL16 :: PreBL16 -> BL16
preBL16ToBL16 PreBL16 {..} = BL16 $ preBlocksToPng preBlocks

preBlocksToPng :: VS.Vector Word8 -> Image PixelRGBA8
preBlocksToPng = Image 64 64 

newLAYRBlockData :: Int -> V.Vector PreBL16 -> V.Vector LAYRBlockData
newLAYRBlockData nextFreeBLIdx preBlocks = V.generate (V.length preBlocks) $ \i ->
  let V3 x y z = fmap fromIntegral $ offset $ preBlocks V.! i
  in  LAYRBlockData { blockIndex = fromIntegral $ nextFreeBLIdx + i
                    , blockX = x
                    , blockY = y
                    , blockZ = z
                    }
  
addLAYRfromBlocks :: V.Vector PreBL16 -> GoxFile -> GoxFile
addLAYRfromBlocks bl16s GoxFile {..} =
  let nextFreeBLIdx = V.length blocks
      layr = newLAYR (newLAYRBlockData nextFreeBLIdx bl16s) (V.length layers + 1)
      newBlocks = preBL16ToBL16 <$> bl16s
  in GoxFile { blocks = blocks V.++ newBlocks
             , layers = layers `V.snoc` layr
             , ..
             }

-- * Conversion between coordinate systems.
      
unsafeVecIdxToVox :: Int -> V3 Int
unsafeVecIdxToVox = unsafePngToVox . unsafeVecIdxToPngCoord

vecIdxToVox :: Int -> V3 Int
vecIdxToVox = pngToVox . vecIdxToPngCoord

unsafeVecIdxToPngCoord :: Int -> (Int, Int)
unsafeVecIdxToPngCoord i =
  let r = i `mod` 4
      idx = i - r
      unscaledIdx = idx `div` 4
      (y, x) = unscaledIdx `divMod` 64
  in (x, y)

vecIdxToPngCoord :: Int -> (Int, Int)
vecIdxToPngCoord i =
  if not ( 0 <= i && i < 16384 )
  then error "'i' not in bounds."
  else 
    let r = i `mod` 4
        idx = i - r    -- base index (index of r if 'i' was for r, g, b, or a)
        unscaledIdx = case idx `divMod` 4 of
                        (d, 0) -> d
                        (_, r) -> error $ "Expected remainder '0' but got '" ++ show r ++ "'."
        (x, y) = let (y, r) = unscaledIdx `divMod` 64
                 in if not (0 <= y && y < 64)
                    then error $ "Expected '0 <= y && y < 64' but got 'y == " ++ show y ++ "'."
                    else if not (0 <= r && r < 64)
                         then error $ "Expected '0 <= r && r < 64' but got 'r == " ++ show r ++ "'."
                         else (r, y)
    in (x, y)

unsafePngToVox :: (Int, Int) -> V3 Int
unsafePngToVox (r, d) =
  let p = d * 64 + r
      (z, xy) = p `divMod` 256
      (y, x) = xy `divMod` 16
  in V3 x y z

pngToVox :: (Int, Int) -> V3 Int
pngToVox (r, d) =
  if not ( 0 <= r && r < 64
           && 0 <= d && d < 64 )
  then error "'r' or 'd' not in bounds."
  else
    let p = d * 64 + r
        (xy, z) = let (z, xy) = p `divMod` 256
                  in if not (0 <= z && z < 64)
                     then error $ "Expected '0 <= z && z < 64' but got '" ++ show z ++ "'."
                     else (xy, z)
        (x, y) = let (y, x) = xy `divMod` 16
                 in if not (0 <= y && y < 16)
                    then error $ "Expected '0 <= y && y < 16' but got '" ++ show y ++ "'."
                    else if not (0 <= x && x < 16)
                         then error $ "Expected '0 <= x && x < 16' but got '" ++ show x ++ "'."
                         else (x, y)
    in V3 x y z

voxToVecIdx :: V3 Int -> Int
voxToVecIdx v@(V3 x y z) =
  if not 
     ( 0 <= x
       && x < 16
       && 0 <= y
       && y < 16
       && 0 <= z
       && z < 16 )
  then error "coord not in bounds"
  else unsafeVoxToVecIdx v

unsafeVoxToVecIdx :: V3 Int -> Int
unsafeVoxToVecIdx = unsafePngToVecIdx . unsafeVoxToPngCoord

voxToPngCoord :: V3 Int -> (Int, Int)
voxToPngCoord v@(V3 x y z) =
  if not
     ( 0 <= x
       && x < 16
       && 0 <= y
       && y < 16
       && 0 <= z
       && z < 16 )
  then error "coord not in bounds"
  else unsafeVoxToPngCoord v

unsafeVoxToPngCoord :: V3 Int -> (Int, Int)
unsafeVoxToPngCoord (V3 x y z) = swap $ (x + (y * 16) + (z * 256)) `divMod` 64

pngToVecIdx :: (Int, Int) -> Int
pngToVecIdx v@(x, y) =
  if not
     ( 0 <= x
       && x < 64
       && 0 <= y
       && y < 64
     )
  then error "coord not in bounds"
  else unsafePngToVecIdx v

unsafePngToVecIdx :: (Int, Int) -> Int
unsafePngToVecIdx (x, y) = (x + y * 64) * 4
