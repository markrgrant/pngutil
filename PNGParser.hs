{-# LANGUAGE OverloadedStrings #-}

module PNGParser where

import Prelude hiding (take)
import Data.Attoparsec.ByteString (Parser, take, string, anyWord8,
                                        manyTill, parse, Result)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Word (Word32)
import Data.Binary.Get (runGet, getWord32be, getWord8)
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


-- parses an IHDR chunk.  This chunk must immediately follow the PNG
-- signature and must always be present.
ihdrParser :: Parser IHDRChunk
ihdrParser = do
    l <- lengthParser
    t <- string "IHDR"
    w <- lengthParser
    h <- lengthParser
    b@(BitDepth bd) <- bitDepthParser
    c <- colorTypeParser
    let maybeDepths = lookup c bitDepths
    case maybeDepths of
        Nothing -> fail "bit depths not found for color type"
        Just depths -> if bd `notElem` depths
            then fail "invalid bit depth for color type"
            else do
                m <- compressionMethodParser
                f <- filterMethodParser
                i <- interlaceMethodParser
                crc <- crcParser
                return IHDRChunk {
                    ihdrLength=l, ihdrChunkType=IHDRChunkType (B.unpack t), 
                    ihdrWidth=w, ihdrHeight=h, ihdrBitDepth=b, ihdrColorType=c,
                    ihdrCompressionMethod=m, ihdrFilterMethod=f,
                    ihdrInterlaceMethod=i, ihdrCRC=crc}


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
    x <- lengthParser
    atype <- take 4 
    d <- take $ fromIntegral x
    crc <- crcParser
    return ANCIChunk {anciLength=x, anciType=atype, anciData=d, anciCRC=crc}


iendParser :: Parser IENDChunk
iendParser = do
    len <- lengthParser
    _ <- string "IEND"
    crc <- crcParser
    return IENDChunk {iendLength=len, iendCRC=crc}


idatParser :: Parser IDATChunk
idatParser = do
    x <- lengthParser
    _ <- string $ "IDAT"
    d <- take $ fromIntegral x
    crc <- crcParser
    return IDATChunk {idatLength = x, idatData = d, idatCRC = crc}


crcParser :: Parser Word32
crcParser = do
    str <- take 4
    return $ runGet getWord32be $ L.fromStrict str


-- parse the color type of the image
colorTypeParser :: Parser ColorType
colorTypeParser = do
    c <- anyWord8
    case c of
        0 -> return Grayscale
        2 -> return Truecolor
        3 -> return IndexedColor
        4 -> return GrayscaleWithAlpha
        6 -> return TruecolorWithAlpha
        _ -> fail $ "unrecognized color type " ++ (show c)


-- Parse a bit depth.  It is not necessarily valid.
bitDepthParser :: Parser BitDepth
bitDepthParser = do
    b <- anyWord8
    if b `notElem` [1,2,4,8,16]
        then fail "invalid bit depth"
        else return $ BitDepth $ fromIntegral b 


sigParser :: Parser B.ByteString
sigParser = string $ B.pack pngSignature
