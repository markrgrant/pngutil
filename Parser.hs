{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString as A
import PNG
import Data.Word
import Data.Binary.Get (runGet, getWord32be, getWord8)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

parseFromFile :: FilePath -> IO (Result PNGDataStream)
parseFromFile fp = B.readFile fp >>= \contents -> 
    return $ parsePNGDataStream contents

-- parse a PNG data stream (such as a PNG file) into a 
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
    str <- A.take 4
    return $ runGet getWord32be (L.fromStrict str)


-- parses an IHDR chunk.  This chunk must immediately follow the PNG
-- signature and must always be present.
ihdrParser :: Parser IHDRChunk
ihdrParser = do
    _ <- lengthParser
    _ <- string "IHDR"
    w <- lengthParser
    h <- lengthParser
    c <- colorTypeParser
    b@(BitDepth bd) <- bitDepthParser c
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
                    ihdrWidth=w, ihdrHeight=h, ihdrBitDepth=b, ihdrColorType=c,
                    ihdrCompressionMethod=m, ihdrFilterMethod=f,
                    ihdrInterlaceMethod=i, ihdrCRC=crc}


filterMethodParser :: Parser FilterMethod
filterMethodParser = do
    f <- A.take 1
    case f of
        "0" -> return DefaultFilterMethod
        _ -> fail "unrecognized filter method"


interlaceMethodParser :: Parser InterlaceMethod
interlaceMethodParser = do
    i <- A.take 1
    case i of
        "0" -> return Null
        "1" -> return Adam7
        _ -> fail "unrecognized interlace method"


compressionMethodParser :: Parser CompressionMethod
compressionMethodParser = do
    m <- A.take 1
    case m of
        "0" -> return DeflateInflate
        _ -> fail "unrecognized compression method"


-- Parse an ancillary chunk.  These are chunks whose ancillary bit of their
-- chunk type is lowercase (bit 5 is 1).
anciParser :: Parser ANCIChunk
anciParser = do
    x <- lengthParser
    atype <- A.take 4 
    d <- A.take $ fromIntegral x
    crc <- crcParser
    return ANCIChunk {anciLength=x, anciType=atype, anciData=d, anciCRC=crc}


iendParser :: Parser ()
iendParser = lengthParser >> string "IEND" >> return ()


idatParser :: Parser IDATChunk
idatParser = do
    x <- lengthParser
    _ <- string $ "IDAT"
    d <- A.take $ fromIntegral x
    crc <- crcParser
    return IDATChunk {idatLength = x, idatData = d, idatCRC = crc}


crcParser :: Parser Word32
crcParser = do
    str <- A.take 4
    return $ runGet getWord32be (L.fromStrict str)


-- parse the color type of the image
colorTypeParser :: Parser ColorType
colorTypeParser = do
    c <- A.anyWord8
    case c of
        0 -> return Grayscale
        2 -> return Truecolor
        3 -> return IndexedColor
        4 -> return GrayscaleWithAlpha
        6 -> return TruecolorWithAlpha
        _ -> fail "unrecognized color type"



-- Parse a bit depth.  It is not necessarily valid.
bitDepthParser :: ColorType -> Parser BitDepth
bitDepthParser ct = do
    let maybeDepths = lookup ct bitDepths
    case maybeDepths of
        Nothing -> fail "color type not recognized"
        Just depths -> do
            b <- A.anyWord8
            if b `notElem` depths
                then fail "invalid bit depth for color type"
                else return $ BitDepth $ fromIntegral b 


sigParser :: Parser B.ByteString
sigParser = string pngSignature
