{-# LANGUAGE OverloadedStrings #-}

-- This module provides constructors for the various types that are 
-- defined in the PNG specification.  These constructors return either
-- a string describing the first error encountered in the input, or the value
-- of the desired type.

module PNG where

import Util (computeCRC)

import Data.Binary.Get (runGet, getWord32be, getWord8)
import Data.Binary (Binary, put, get)
import Data.Word (Word8, Word32)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B



---------------------------- Reference Image ---------------------------------
-- A reference image type.  It is an abstraction capable of representing any
-- image (grayscale, trucolor with alpha, etc.).  Different channels can
-- have different sample depths.

data RefImg = RefImg {
    refWidth :: Word32,
    refHeight :: Word32,
    refColorType :: ColorType,
    refRed :: Channel,
    refBlue :: Channel,
    refGreen :: Channel,
    refAlpha :: Channel
}

-- A channel is a uniform color with an intensity given by a sample depth, 
-- which is the number of bits used to represent the color.
data Channel = Channel {
    channelDepth :: BitDepth, -- between 1 and 16 bits
    channelSamples :: B.ByteString
}


replaceLeft :: Either String a -> String -> Either String a
replaceLeft (Right x) msg = Right x
replaceLeft (Left x) msg = Left msg


createRefImg :: Word32 -> Word32 -> Word8 -> Word8 -> Word8 ->
    Word8 -> Word8 -> Either String RefImg
createRefImg w h c r g b a = do
    vw <- replaceLeft (createLength w) "width must be > 0"
    vh <- replaceLeft (createLength h) "height must be > 0"
    vc <- (createColorType c)
    vr <- replaceLeft (createChannel vw vh vc r) "red bit depth is invalid"
    vg <- replaceLeft (createChannel vw vh vc g) "green bit depth is invalid"
    vb <- replaceLeft (createChannel vw vh vc b) "blue bit depth  is invalid"
    va <- replaceLeft (createChannel vw vh vc a) "alpha bit depth is invalid"
    return RefImg {refWidth=w, refHeight=h,refColorType=vc, refRed=vr,
        refGreen=vg, refBlue=vb, refAlpha=va}


createColorType :: Word8 -> Either String ColorType
createColorType c =
    case c of
        0 -> return Grayscale
        2 -> return Truecolor
        3 -> return IndexedColor
        4 -> return GrayscaleWithAlpha
        6 -> return TruecolorWithAlpha
        _ -> Left "unrecognized color type"




-- A length is an integral value greater than zero
createLength :: Word32 -> Either String Word32
createLength x = if x <= 0
    then Left "length must be > 0"
    else Right x


createChannel :: Word32 -> Word32 -> ColorType -> Word8 -> Either String Channel
createChannel w h ct d = 
    let maybeDepths = lookup ct bitDepths
    in case maybeDepths of
        Nothing -> Left "color type not found"
        Just depths -> if d `notElem` depths
                           then Left "invalid depth for color type"
                           else Right $ Channel {
                               channelDepth=BitDepth d,
                               channelSamples=B.replicate (iw*ih*id `div` 8) 1}
    where iw = fromIntegral w 
          ih = fromIntegral h 
          id = fromIntegral d 

--------------------------- PNG Image ---------------------------------------

-- A data type containing the necessary information for producing a PNG data
-- stream.
data PNGImg = TruecolorWithAlphaImg | GrayscaleWithAlphaImg | TruecolorImg |
    GrayscaleImg | IndexedColorImg deriving (Show)



--------------------------- PNG Data Stream ---------------------------------

-- A data type for representing the results from parsing any PNG data stream. 
-- It excludes the PNG signature that starts every stream and the IEND chunk
-- that ends it, as these are the same for every PNG data stream and contain
-- no additional information, and are added during serialization.
data PNGDataStream = PNGDataStream IHDRChunk [IDATChunk] deriving (Show)

-- defines how a PNG data stream is serialized into bytes. 
instance Binary PNGDataStream where
    get = undefined
    put (PNGDataStream ihdr idats) = do
        put (PNGSig pngSignature)
        put ihdr
        put idats
        put $ IENDChunk {iendLength=0, iendCRC=0}


createPNGDataStream :: PNGDataStream
createPNGDataStream = undefined


data Palette = Palette [(Red, Green, Blue)] AlphaTable

type AlphaTable = [(Alpha, Red, Blue, Green)]
type Alpha = Int
type Red = Int
type Blue = Int
type Green = Int

-- Convert a PNG to a bytestring
-- The steps in encoding are:
-- 1.  Pass extraction
-- 2.  Scanline serialization
-- 3.  Filtering
-- 4.  Compression
-- 5.  Chunking
-- 6.  Datastream construction
encodePNG :: PNGImg -> B.ByteString
encodePNG = passExtraction >> scanlineSerialization >> filtering >>
    compression >> chunking >> datastreamConstruction


-- Pass extraction splits a PNG image into a sequence of reduced images (the
-- interlaced PNG image) where the first image defines a coarse view and
-- subsequent images enhance this coarse view until the last image completes
-- the PNG image. This allows progressive display of the interlaced PNG image
-- by the decoder and allows images to "fade in" when they are being displayed
-- on-the-fly. On average, interlacing slightly expands the datastream size,
-- but it can give the user a meaningful display much more rapidly.
passExtraction = undefined


scanlineSerialization = undefined
filtering = undefined
compression = undefined
chunking = undefined
datastreamConstruction = undefined

-- Alpha channel separation is the examination of the alpha channel to
-- determine if all of the values are maximum (2^(sample depth) - 1).  If so,
-- then the alpha channel can be removed.
--
-- This step is not required, and some implementations may not wish to
-- use it as it removes an alpha channel (even though it is contributing
-- nothing).
alphaSep :: PNGImg -> PNGImg
alphaSep = undefined


-- Perform indexing.  This step is not required.  If it is performed, it is
-- done in place of rgb merging and alpha compaction.  If it is not performed,
-- then rgb merging and alpha composition must be performed.  Indexing is
-- the creation of a palatte of color values, and a single array of 8-bit
-- values that contain indices into the palette.  If an alpha channel is
-- present there is also a parallel table of alpha values.
--
-- The alpha table and palette can be reordered to put those pixels with
-- maximum alpha values at the bottom, and then the alpha table can be
-- shortened and not include these values.
--
-- Index tables may be included even if indexing isn't used to encode the
-- image, to assist with color mapping if a display has limited colors.
indexing :: PNGImg -> PNGImg
indexing = undefined


-- If the RGB channels all have the same sample depth, and for every pixel
-- the red, green, and blue values are the same, merge the channels into
-- a single grayscale channel.
-- If this step is performed, then it must be followed
-- by alpha compaction.
rgbMerging :: PNGImg -> PNGImg
rgbMerging = undefined


-- For non-indexed images, if there exists an RGB (or greyscale) value such
-- that all pixels with that value are fully transparent while all other
-- pixels are fully opaque, then the alpha channel can be represented more
-- compactly by merely identifying the RGB (or greyscale) value that is
-- transparent.
--
-- This step is always preceded by rgb merging.
alphaComp :: PNGImg -> PNGImg
alphaComp = undefined


-- Perform sample depth scaling.  This is the last step in conversion of
-- a reference image into a PNG image. It is always performed.
depthScale :: PNGImg -> PNGImg
depthScale = undefined


-- Returns true if indexing would lead to a smaller image.   Indexing should
-- be used if only if:
-- 1. The number of distinct pixel values (rgb(a) combinations) is 256 or less.
-- 2. All rgb sample depths are not greater than 8
-- 3. The alpha channel sample depth is absent, or is 8, or all values are
--    either maximum or 0.
shouldIndex :: PNGImg -> Bool
shouldIndex = undefined


-------------------------- Color Type ---------------------------------------

-- a single-byte integer that defines the PNG image type.  Only certain
-- bit depths are allowed for a given color type:
-- PNG image type       PNG color type          allowed bit depths
-- grayscale            0                       1,2,4,8,16
-- truecolor            2                       8,16
-- indexed color        3                       1,2,4,8
-- grayscale with alpha 4                       8,16
-- truecolor with alpha 6                       8,16
--
data ColorType = Grayscale |
                 Truecolor |
                 IndexedColor |
                 GrayscaleWithAlpha |
                 TruecolorWithAlpha deriving (Eq)


instance Show ColorType where
    show Grayscale = "grayscale"
    show Truecolor = "truecolor"
    show IndexedColor = "indexed color"
    show GrayscaleWithAlpha = "grayscale with alpha"
    show TruecolorWithAlpha = "truecolor with alpha"


instance Binary ColorType where
    put c = case c of
                Grayscale -> put (0 :: Word8)
                Truecolor -> put (2 :: Word8)
                IndexedColor -> put (3 :: Word8)
                GrayscaleWithAlpha -> put (4 :: Word8)
                TruecolorWithAlpha -> put (6 :: Word8)
    get = undefined


------------------------------ Bit Depth -------------------------------------

-- For IndexedColor images, it is the number of bits per palette index.  For
-- other images it is the number of bits per sample in the image (a sample
-- being a pixel value in a particular color channel).

-- The allowed bit depths for a given ColorType
bitDepths :: [(ColorType, [Word8])]
bitDepths = [(Grayscale,[1,2,4,8,16]),
             (Truecolor,[8,16]),
             (IndexedColor,[1,2,4,8]),
             (GrayscaleWithAlpha,[8,16]),
             (TruecolorWithAlpha,[8,16])]


-- The PNG signature is an 8 byte value that must be present at the start
-- of all valid PNG data streams.


---------------------------- PNG Signature ---------------------------------

data PNGSig = PNGSig [Word8]

instance Binary PNGSig where
    get = undefined
    put (PNGSig words) = do
        put $ words!!0
        put $ words!!1
        put $ words!!2
        put $ words!!3
        put $ words!!4
        put $ words!!5
        put $ words!!6
        put $ words!!7

pngSignature :: [Word8]
pngSignature = [137,80,78,71,13,10,26,10]::[Word8]

-------------------------------- Chunks ------------------------------------

-- A PNG data stream consists of 

-- a chunk consists of:
--
-- length - a four byte unsigned int giving the number of bytes in the chunk's
--          data field.
--
-- chunk type - a four byte sequence defining the chunk type.  Each byte
--              of a chunk type is restricted to the decimal values 65 to 90
--              and 97 to 12.  These correspond to the uppercase and lowercase
--              ISO 646 letters (A-Z and a-z) respectively, for convenience
--              in description and examination of PNG datastreams.  The chunk
--              types should be treated as strict binary values not by their
--              ISO 646 letter equivalents.
--
--              critical chunk types:
--              IHDR - image header, first chunk in PNG datastream
--              PLTE - palette table for indexed PNG images.
--              IDAT - image data chunks
--              IEND - image trailer, last chunk in a PNG data stream
--
--              ancillary chunk types:
--              tRNS - transparency information
--              cHRM, gAMA, iCCP, sBIT, sRGB - color space information
--              iTXt, tEXt, zTXt - textual information
--              bKGD, hIST, pHYs, sPLT - miscellaneous informaiton
--              tIME - timestamp information
--
-- chunk data - the data bytes appropriate to the chunk type, if any.  This
--              field can be of zero length.
-- chunk CRC - a four byte cyclic redundancey code calaculated on the
--             preceding bytes in the chunk, including the chunk type field
--             and chunk data fields but not including the length field.
--
data Chunk = Chunk {
    chunkLength :: Int,
    chunkType :: ChunkType, 
    chunkData :: B.ByteString,
    chunkCRC :: Word32
}


data ChunkType = IHDRType | IFTRType



-- A valid bit depth has the value 1, 2, 4, 8, or 16. 
data BitDepth = BitDepth Word8 deriving (Eq, Ord)

instance Show BitDepth where
    show (BitDepth d) = show d


instance Binary BitDepth where
    get = undefined
    put (BitDepth depth) = put depth

---------------------------- IHDR Chunk --------------------------------------

-- The first chunk in all png data streams
data IHDRChunk = IHDRChunk {
    ihdrLength :: Word32,
    ihdrChunkType :: IHDRChunkType,
    ihdrWidth :: Word32,                  -- image width in pixels
    ihdrHeight :: Word32,                 -- image height in pixels
    ihdrBitDepth :: BitDepth,             -- bits per sample or pallete index.
    ihdrColorType :: ColorType,
    ihdrCompressionMethod :: CompressionMethod,
    ihdrFilterMethod :: FilterMethod,
    ihdrInterlaceMethod :: InterlaceMethod,
    ihdrCRC :: Word32
}



-- The only compression method defined in the PNG standard is deflate/inflate
-- compression with a sliding window of at most 32768 bytes
data CompressionMethod = DeflateInflate deriving (Eq)

instance Show CompressionMethod where
    show DeflateInflate = "deflate/inflate"

instance Binary CompressionMethod where
    get = undefined
    put method = put (0 :: Word8)


-- only one filter method is defined
data FilterMethod = AdaptiveFiltering


instance Binary FilterMethod where
    get = undefined
    put method = put (0 :: Word8)

instance Show FilterMethod where
    show AdaptiveFiltering = "adaptive filtering"


data InterlaceMethod = Null | Adam7 deriving (Eq)

instance Show InterlaceMethod where
    show Null = "none"
    show Adam7 = "Adam7"

instance Binary InterlaceMethod where
    get = undefined
    put Null = put (0 :: Word8)
    put Adam7 = put (1 :: Word8)



data IHDRChunkType = IHDRChunkType [Word8]

ihdrBytes :: [Word8]
ihdrBytes = [73, 72, 68, 82]


instance Binary IHDRChunkType where
    get = undefined
    put (IHDRChunkType words) = do
        put $ words!!0 
        put $ words!!1
        put $ words!!2
        put $ words!!3


instance Binary IHDRChunk where
    put ihdr = do
        put $ ihdrLength ihdr
        put $ ihdrChunkType ihdr
        put $ ihdrWidth ihdr
        put $ ihdrHeight ihdr
        put $ ihdrBitDepth ihdr
        put $ ihdrColorType ihdr
        put $ ihdrCompressionMethod ihdr
        put $ ihdrFilterMethod ihdr
        put $ ihdrInterlaceMethod ihdr
        put $ ihdrCRC ihdr
        -- TODO: calculate CRC
        -- put $ computeCRC $ B.concat [c,w,h,d,t,cm,fm,im]
    get = undefined

instance Show IHDRChunk where
    show c = "(IHDRChunk " ++
             "length=" ++ show (ihdrLength c) ++
             ", w=" ++ show (ihdrWidth c) ++
             ", h=" ++ show (ihdrHeight c) ++
             ", bit depth=" ++ show (ihdrBitDepth c) ++
             ", color type=" ++ show (ihdrColorType c) ++
             ", compression method=" ++ show (ihdrCompressionMethod c) ++
             ", filter method=" ++ show (ihdrFilterMethod c) ++
             ", interlace method=" ++ show (ihdrInterlaceMethod c) ++
             ", crc=" ++ show (ihdrCRC c) ++
             ")"

------------------------------ IDAT Chunk -------------------------------------
data IDATChunk = IDATChunk {
    idatLength :: Word32,
    idatData :: B.ByteString,
    idatCRC :: Word32} 

instance Show IDATChunk where
    show c = "(IDATChunk " ++
             "length=" ++ (show (idatLength c)) ++
             ", crc=" ++ (show (idatCRC c)) ++ 
             ")"


instance Binary IDATChunk where
    get = undefined -- using attoparsec
    put idat = do
        put $ idatLength idat
        put ("IDAT"::B.ByteString)
        put $ idatData idat
        put $ idatCRC idat


------------------------------ IEND Chunk ------------------------------------

data IENDChunk = IENDChunk {iendLength::Word32, iendCRC::Word32}

instance Show IENDChunk where
    show chunk = "(IENDChunk " ++
                     "length=" ++ show (iendLength chunk) ++
                     ", crc=" ++ show (iendCRC chunk) ++
                     ")"

iend :: B.ByteString
iend = "IEND"


instance Binary IENDChunk where
    get = undefined
    put chunk = do
        put $ iendLength chunk -- the length is zero
        put iend
        put $ iendCRC chunk   -- TODO: is the crc zero?


----------------------------- Ancillary Chunks -------------------------------
-- A data type for representing ancillary chunks.  Ancillary chunks provide
-- additional information about an image that are generated by encoders and
-- interpreted and possibly used by decoders.  The ancillary chunk types are:
-- tRNS - transparency information
-- cHRM -
-- gAMA -
-- iCCP -
-- sBIT -
-- sRGB -
-- iTXt -
-- tEXt -
-- zTXt -
-- bKGD -
-- hIST -
-- pHYs -
-- sPLT -
-- tIME -
data ANCIChunk = ANCIChunk {
    anciLength :: Word32,
    anciType :: B.ByteString,
    anciData :: B.ByteString,
    anciCRC :: Word32
}
instance Show ANCIChunk where
    show c = "(ANCIChunk length=" ++ (show (anciLength c)) ++ ")"
