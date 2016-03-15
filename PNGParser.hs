{-# LANGUAGE OverloadedStrings #-}

module PNGParser where

import Prelude hiding             (take, count)
import Data.Attoparsec.ByteString (Parser, take, string, anyWord8,
                                   manyTill, parse, Result, count)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word                  (Word32, Word8)
import Data.Binary.Get            (runGet, getWord32be, getWord8)

import CRC32                      (crc32)
import PNG


parseFromFile :: FilePath -> IO (Result PNGDataStream)
parseFromFile fp = B.readFile fp >>= \contents -> 
    return $ parsePNGDataStream contents


-- lazily parse a PNG data stream (such as a PNG file) into a 
-- format that references the important parts of the data stream
parsePNGDataStream :: B.ByteString -> Result PNGDataStream
parsePNGDataStream = parse pngDataStreamParser


-- parses a byte stream into a PNG image
pngDataStreamParser :: Parser PNGDataStream
pngDataStreamParser = do
    _    <- sigParser -- discard the PNG signature after it matches
    ihdr <- ihdrParser
    -- anci <- anciParser
    ds   <- manyTill idatParser iendParser
    --return $ PNGDataStream ihdr anci ds
    return $ PNGDataStream ihdr ds


lengthParser :: Parser Word32
lengthParser = do
    str <- take 4
    return $ runGet getWord32be $ L.fromStrict str


crcBytesParser :: Word32 -> Parser B.ByteString
crcBytesParser n = lookAhead $ take (fromIntegral n)


-- parses an IHDR chunk.  This chunk must immediately follow the PNG
-- signature and must always be present.
ihdrParser :: Parser IHDRChunk
ihdrParser = do
    l <- lengthParser -- should always be the number 13
    crcData <- crcBytesParser (l+4) -- the chunk type is included in crc bytes
    t <- string "IHDR"
    w <- lengthParser
    h <- lengthParser
    b <- bitDepthParser
    c <- colorTypeParser b
    m <- compressionMethodParser
    f <- filterMethodParser
    i <- interlaceMethodParser
    crc <- crcParser crcData
    return IHDRChunk {
        ihdrLength=l,
        ihdrChunkType=IHDRChunkType (B.unpack t), 
        ihdrWidth=w,
        ihdrHeight=h,
        ihdrBitDepth=b,
        ihdrColorType=c,
        ihdrCompressionMethod=m,
        ihdrFilterMethod=f,
        ihdrInterlaceMethod=i,
        ihdrCRC=crc}


filterMethodParser :: Parser FilterMethod
filterMethodParser = do
    f <- anyWord8
    case f of
        0 -> return AdaptiveFiltering
        _ -> fail "unrecognized filter method"


interlaceMethodParser :: Parser InterlaceMethod
interlaceMethodParser = do
    i <- anyWord8
    case i of
        0 -> return Null
        1 -> return Adam7
        _ -> fail "unrecognized interlace method"


compressionMethodParser :: Parser CompressionMethod
compressionMethodParser = do
    m <- anyWord8
    case m of
        0 -> return DeflateInflate
        _ -> fail "unrecognized compression method "


-- Parse an ancillary chunk.  These are chunks whose ancillary bit of their
-- chunk type is lowercase (bit 5 is 1).
anciParser :: Parser ANCIChunk
anciParser = do
    l <- lengthParser
    crcData <- crcBytesParser (l+4) -- for verifying crc
    atype <- take 4 
    d <- take $ fromIntegral l
    crc <- crcParser crcData
    return ANCIChunk {anciLength=l, anciType=atype, anciData=d, anciCRC=crc}


idatParser :: Parser IDATChunk
idatParser = do
    l <- lengthParser
    crcData <- crcBytesParser (l+4)
    t <- string $ "IDAT"
    d <- take $ fromIntegral l
    crc <- crcParser crcData
    return IDATChunk {
        idatLength = l,
        idatChunkType=IDATChunkType (B.unpack t), 
        idatData = d,
        idatCRC = crc}


iendParser :: Parser IENDChunk
iendParser = do
    l <- lengthParser
    crcData <- crcBytesParser (l+4)
    t <- string "IEND"
    crc <- crcParser crcData
    return IENDChunk {
        iendLength=l,
        iendChunkType=IENDChunkType (B.unpack t),
        iendCRC=crc}


crcParser :: B.ByteString -> Parser Word32
crcParser dat = do
    str <- count 4 anyWord8
    let crcFound = runGet getWord32be (L.pack str)
        crcComputed = crc32 $ B.unpack dat
    if crcFound /= crcComputed
        then fail $ "crc error. computed = " ++ (show crcComputed) ++
            ", found = " ++ (show crcFound)
        else return crcFound


-- parse the color type of the image
colorTypeParser :: BitDepth -> Parser ColorType
colorTypeParser (BitDepth d) = do
    c <- anyWord8
    colorType <- do
        case c of
            0 -> return Grayscale
            2 -> return Truecolor
            3 -> return IndexedColor
            4 -> return GrayscaleWithAlpha
            6 -> return TruecolorWithAlpha
            _ -> fail $ "unrecognized color type " ++ (show c)
    let maybeDepths = lookup colorType bitDepths
    case maybeDepths of
        Nothing -> fail "color type not found"
        Just depths -> do
            if (fromIntegral d) `notElem` depths
                then fail "bit depth not valid for color type"
                else return colorType
    

-- Parse a bit depth.  It is not necessarily valid.
bitDepthParser :: Parser BitDepth
bitDepthParser = do
    b <- anyWord8
    if b `notElem` [1,2,4,8,16]
        then fail "invalid bit depth"
        else return $ BitDepth $ fromIntegral b 


sigParser :: Parser B.ByteString
sigParser = string $ B.pack pngSignature
