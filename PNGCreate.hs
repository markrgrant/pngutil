{-# LANGUAGE OverloadedStrings #-}

import PNG
import PNGParser
import System.Environment (getArgs)
import Data.Word (Word32, Word8)
import Text.Read (readMaybe)
import qualified Data.Binary as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Attoparsec.ByteString as A


-- Given a description of a PNG image, in particular its size and channel
-- bit depth, produce a data stream defining a valid PNG image with the
-- desired specifications. 
pngcreate :: RefImg-> PNGDataStream
pngcreate refimg = 
    let bitdepth = maximum [
            channelDepth (refRed refimg),
            channelDepth (refGreen refimg),
            channelDepth (refBlue refimg),
            channelDepth (refAlpha refimg)]
        ihdr = IHDRChunk {
            ihdrWidth=refWidth refimg,
            ihdrHeight=refHeight refimg,
            ihdrBitDepth=bitdepth,
            ihdrColorType=Grayscale,
            ihdrCompressionMethod=DeflateInflate,
            ihdrFilterMethod=DefaultFilterMethod,
            ihdrInterlaceMethod=Null,
            ihdrCRC=0
        }
        idat = IDATChunk {
            idatLength=0,
            idatData=B.empty,
            idatCRC=0}
    in
        PNGDataStream ihdr [idat]


data Args = Args {
    argsWidth :: Word32,
    argsHeight :: Word32,
    argsColorType :: ColorType,
    argsRedDepth :: BitDepth,
    argsGreenDepth :: BitDepth,
    argsBlueDepth :: BitDepth,
    argsAlphaDepth :: BitDepth
} deriving (Show)


argsParser :: [String] -> Either String Args
argsParser args = do
    _ <- assertEither (length args == 7) "invalid # of arguments"
    w  <- resultToEither $ parse lengthParser (LB.toStrict (DB.encode (read (args!!0)::Word32)))
    h  <- resultToEither $ parse lengthParser (LB.toStrict (DB.encode (read (args!!1)::Word32)))
    ct <- resultToEither $ parse colorTypeParser (LB.toStrict (DB.encode (read (args!!2)::Word8)))
    rd <- resultToEither $ parse (bitDepthParser ct) (LB.toStrict (DB.encode (read (args!!3)::Word8)))
    gd <- resultToEither $ parse (bitDepthParser ct) (LB.toStrict (DB.encode (read (args!!4)::Word8)))
    bd <- resultToEither $ parse (bitDepthParser ct) (LB.toStrict (DB.encode (read (args!!5)::Word8)))
    ad <- resultToEither $ parse (bitDepthParser ct) (LB.toStrict (DB.encode (read (args!!6)::Word8)))
    return Args {argsWidth=w, argsHeight=h, argsColorType=ct,
                 argsRedDepth=rd, argsGreenDepth=gd, argsBlueDepth=bd,
                 argsAlphaDepth=ad}


assertEither :: Bool -> String -> Either String Bool
assertEither b msg = if b then Right b else Left msg


resultToEither :: IResult B.ByteString a -> Either String a
resultToEither (Done _ r) = Right r
resultToEither (Fail _ _ msg)  = Left msg
resultToEither (Partial f) = resultToEither $ f ""

usage = "\nusage: pngcreate width height colortype reddepth greendepth " ++
        "bluedepth alphadepth\n\n" ++ 
        "where:\n\n" ++
        "* width is the width of the image in pixels, must be > 0\n" ++
        "* height is the height of the image in pixels, must be > 0\n" ++
        "* colortype is the color type of the image, where 0 is grayscale,\n" ++
        "  2 is truecolor, 3 is indexed color, 4 is grayscale with alpha,\n" ++ 
        "  and 6 is truecolor with alpha\n" ++
        "* reddepth, greendepth, bluedepth, and alphadepth are the bit\n" ++
        "  depths to use for the corresponding color channel. Allowed bit\n" ++
        "  depths depend on the color type: \n\n" ++
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
    let maybeArgs = argsParser argStrings
    case maybeArgs of
        Left msg -> do
            putStrLn msg
            putStrLn usage
        Right args -> do
            let w = argsWidth args
                h = argsHeight args
                ct = argsColorType args
                r@(BitDepth rd) = argsRedDepth args
                g@(BitDepth gd) = argsGreenDepth args
                b@(BitDepth bd) = argsBlueDepth args
                a@(BitDepth ad) = argsAlphaDepth args
                iw = fromIntegral w
                ih = fromIntegral h
                ir = fromIntegral rd
                ig = fromIntegral gd
                ib = fromIntegral bd
                ia = fromIntegral ad
                refimg = RefImg {
                    refWidth=w,
                    refHeight=h,
                    refColorType=ct,
                    refRed=Channel{
                        channelDepth=r,
                        channelSamples=B.replicate (iw*ih*ir) 1},
                    refGreen=Channel{
                        channelDepth=g,
                        channelSamples=B.replicate (iw*ih*ig) 1},
                    refBlue=Channel{
                        channelDepth=b,
                        channelSamples=B.replicate (iw*ih*ib) 1},
                    refAlpha=Channel{
                        channelDepth=a,
                        channelSamples=B.replicate (iw*ih*ia) 1}
                }
            LB.putStrLn $ DB.encode $ pngcreate refimg
