import PNG
import System.Environment (getArgs)
import PNGParser (parseFromFile)
import Data.Attoparsec.ByteString (IResult(Done, Fail))

pngInfo :: PNGDataStream -> String
pngInfo (PNGDataStream ihdr idats) =
    "width              = " ++ show (ihdrWidth ihdr) ++ " pixels" ++
    "\nheight             = " ++ show (ihdrHeight ihdr) ++ " pixels" ++
    "\nbit depth          = " ++ show (ihdrBitDepth ihdr) ++ " bits" ++
    "\ncolor type         = " ++ show (ihdrColorType ihdr) ++
    "\ncompression method = " ++ show (ihdrCompressionMethod ihdr) ++
    "\nfilter method      = " ++ show (ihdrFilterMethod ihdr) ++
    "\ninterlace method   = " ++ show (ihdrInterlaceMethod ihdr) ++
    "\ncrc                = " ++ show (ihdrCRC ihdr) ++
    "\n# of data chunks   = " ++ show (length idats)


usage = "usage: pnginfo filename"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn usage
        else do
            result <- parseFromFile $ head args
            case result of 
                (Done _ datastream) -> putStrLn $ pngInfo datastream
                (Fail _ _ msg)  -> putStrLn msg
