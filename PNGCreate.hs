{-# LANGUAGE OverloadedStrings #-}

import PNG
import Parser
import System.Environment (getArgs)
import Data.Word (Word32, Word8)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as BC


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
    _ <- assert (length args == 7) "invalid # of arguments"
    let pargs = map BC.pack args
    w  <- resultToEither $ parse lengthParser        (pargs!!0)
    h  <- resultToEither $ parse lengthParser        (pargs!!1)
    ct <- resultToEither $ parse colorTypeParser     (pargs!!2)
    rd <- resultToEither $ parse (bitDepthParser ct) (pargs!!3)
    gd <- resultToEither $ parse (bitDepthParser ct) (pargs!!4)
    bd <- resultToEither $ parse (bitDepthParser ct) (pargs!!5)
    ad <- resultToEither $ parse (bitDepthParser ct) (pargs!!6)
    return Args {argsWidth=w, argsHeight=h, argsColorType=ct,
                 argsRedDepth=rd, argsGreenDepth=gd, argsBlueDepth=bd,
                 argsAlphaDepth=ad}


assert :: Bool -> String -> Either String Bool
assert b msg = if b then Right b else Left msg


resultToEither :: IResult BC.ByteString a -> Either String a
resultToEither (Done _ r) = Right r
resultToEither (Fail _ _ msg)  = Left msg
resultToEither (Partial f) = resultToEither $ f ""

usage = "usage: pngcreate width height colortype reddepth greendepth bluedepth alphadepth"


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
                pngstream = pngcreate refimg
            BC.putStrLn "testing"
