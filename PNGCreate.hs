{-# LANGUAGE OverloadedStrings #-}

import PNG
import PNGParser
import System.Environment (getArgs)
import Data.Word (Word32, Word8)
import Text.Read (readMaybe)
import qualified Data.Binary as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Attoparsec.ByteString as A


-- Given a description of a PNG image, in particular its size and channel
-- bit depth, produce a data stream defining a valid PNG image with the
-- desired specifications. 
pngcreate :: RefImg-> PNGDataStream
pngcreate refimg = 
    let ihdr = IHDRChunk {
            -- the length of a chunk only includes the length 
            -- of its data, not length or type or crc.  So for
            -- the ihdr its 4(width)+4(height)+1(bit depth)+
            -- 1(colortype)+1(compressionmethod)+1(filtermethod)
            -- +1(interlacemethod)=13 bytes
            ihdrLength=13, 
            ihdrChunkType=IHDRChunkType ihdrBytes,
            ihdrWidth=refWidth refimg,
            ihdrHeight=refHeight refimg,
            ihdrBitDepth=channelDepth (refRed refimg),
            ihdrColorType=Grayscale,
            ihdrCompressionMethod=DeflateInflate,
            ihdrFilterMethod=AdaptiveFiltering,
            ihdrInterlaceMethod=Null,
            ihdrCRC=0 -- placeholder, this needs to be computedduring encoding
        }
        idat = IDATChunk {
            -- 1(chunk type) + 4 * bit depth * width * height / 8
            idatLength=fromIntegral $ 1 + 4 * B.length (channelSamples (refRed refimg)),
            idatChunkType=IDATChunkType idatBytes,
            idatData=B.empty,
            idatCRC=0 -- real value computed during decoding
        }
    in PNGDataStream ihdr [idat]


-- An type representing the inputs to the pngcreate command
data Args = Args {
    argsWidth :: Word32,
    argsHeight :: Word32,
    argsColorType :: Word8,
    argsBitDepth :: Word8,
    argsFilename :: String
} deriving (Show)


-- perform limited validation of the inputs to ensure all are present and
-- of the appropriate type for further processing
createArgs :: [String] -> Either String Args
createArgs args = do
    assertEither (length args == 5) "invalid number of arguments"
    w <- eitherWord32 (args!!0) "invalid width"
    h <- eitherWord32 (args!!1) "invalid height"
    ct <- eitherWord8 (args!!2) "invalid color type"
    bd <- eitherWord8 (args!!3) "invalid bit depth"
    let fname = args!!4
    return Args {argsWidth=w, argsHeight=h, argsColorType=ct, argsBitDepth=bd,
                 argsFilename=fname}


eitherWord32 :: String -> String -> Either String Word32
eitherWord32 dat msg = case (readMaybe dat::Maybe Word32) of
    Nothing -> Left msg
    Just value -> Right value


eitherWord8 :: String -> String -> Either String Word8
eitherWord8 dat msg = case (readMaybe dat:: Maybe Word8) of
    Nothing -> Left msg
    Just value -> Right value


assertEither :: Bool -> String -> Either String Bool
assertEither b msg = if b then Right b else Left msg


usage = "\nusage: pngcreate width height colortype bitdepth filename\n\n" ++ 
        "where:\n\n" ++
        "* width is the width of the image in pixels, must be > 0\n" ++
        "* height is the height of the image in pixels, must be > 0\n" ++
        "* colortype is the color type of the image, where 0 is grayscale,\n" ++
        "  2 is truecolor, 3 is indexed color, 4 is grayscale with alpha,\n" ++ 
        "  and 6 is truecolor with alpha\n" ++
        "* bitdepth is the bit depth to use for the color channels,\n" ++
        "* filename is the name of the png file to be produced.\n" ++
        "  Allowed bit depths depend on the color type: \n\n" ++
        "  color type             code  allowed bit depths\n" ++
        "  -----------------------------------------------\n" ++
        "  grayscale              0     1,2,4,8,16        \n" ++
        "  truecolor              2     8,16              \n" ++
        "  indexed color          3     1,2,4,8           \n" ++
        "  grayscale with alpha   4     8,16              \n" ++
        "  truecolor with alpha   6     8,16              \n"
        




main :: IO ()
main = do
    argStrings <- getArgs
    let eitherArgs = createArgs argStrings
    case eitherArgs of
        Left msg -> do
            putStrLn msg
            putStrLn usage
        Right args -> do
            let w = argsWidth args
                h = argsHeight args
                ct = argsColorType args
                bd = argsBitDepth args
                eitherRefImg = createRefImg w h ct bd bd bd bd
            case eitherRefImg of
                Left msg -> do
                    putStrLn msg
                    putStrLn usage
                Right refImg -> do
                    DB.encodeFile (argsFilename args) $ pngcreate refImg
