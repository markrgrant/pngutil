import PNG
import CRC32 (crc32)
import Data.ByteString.Lazy as L
import System.Exit (exitSuccess, exitFailure)
import Data.Binary.Put (runPut)
import Data.Binary (put)
import Data.Word (Word8)


testPutIHDRChunk :: Bool
testPutIHDRChunk =
    let ihdr = IHDRChunk {
            ihdrLength=13,
            ihdrChunkType=IHDRChunkType ihdrBytes,
            ihdrWidth=100,
            ihdrHeight=100,
            ihdrBitDepth=(BitDepth 8),
            ihdrColorType=TruecolorWithAlpha,
            ihdrCompressionMethod=DeflateInflate,
            ihdrFilterMethod=AdaptiveFiltering,
            ihdrInterlaceMethod=Null,
            ihdrCRC=0
        }
        ihdrData = [73, 72, 68, 82, 0, 0, 0, 100, 0, 0, 0,
                    100, 8, 6, 0, 0, 0] :: [Word8]
        crc =  crc32 ihdrData
        ihdrByteString = L.pack $ [0,0,0,13] ++ ihdrData ++
                            (L.unpack (runPut (put crc)))
    in runPut (put ihdr) == ihdrByteString


tests :: [Bool]
tests = [testPutIHDRChunk]


runTests tests = do
    if (and tests)
        then exitSuccess
        else do
            print tests
            exitFailure


main :: IO ()
main = do
    runTests tests
    return ()
