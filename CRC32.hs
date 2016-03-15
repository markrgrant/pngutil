module CRC32 (crc32) where


import qualified Data.Vector as V -- for O(1) lookup
import Data.Bits
import Data.Word


-- a table containing all 8-bit messages in order to improve the
-- performance of the calculation of a CRC. 
crcTable :: V.Vector Word64
crcTable = V.fromList $ map makeCRCEntry [0..255]
    where makeCRCEntry n = foldl (\acc _ -> nextCRC acc) n [0..7]
          nextCRC c
              | c .&. 1 == 0 = c `shiftR` 1
              | otherwise = 0xedb88320 `xor` (c `shiftR` 1)


updateCRC :: V.Vector Word64 -> [Word8] -> Word64
updateCRC table bytes = foldl func crc bytes
    where crc = 0xffffffff -- initial crc is all 1's
          func acc byte = (table V.! index) `xor` (acc `shiftR` 8)
              where index = fromIntegral (acc `xor` fromIntegral byte) .&. 0xff :: Int


-- Return the 1's complement of the computed crc
crc32 :: [Word8] -> Word32
crc32 bytes = fromIntegral (updateCRC crcTable bytes `xor` 0xffffffff) ::Word32
