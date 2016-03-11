module Util where

import Data.ByteString as B
import Data.Word (Word32)

-- As described in the PNG specification: 
--
-- computed as defined by ISO 3309 and ITU-T V.42.  The CRC polynomial 
-- employed is x^32+x^26+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1
--
-- in PNG, the 32 bit CRC is initialized to all 1's, and then the data from
-- each byte is processed from the least significant bit (1) to the most
-- significant bit (128).  After all data bytes are processed, the CRC is
-- inverted (its ones complement is taken).  This value is transmitted
-- (stored in the datastream) MSB first.  For the purpose of separating
-- into bytes and ordering, the least significant bit of the 32-bit CRC is
-- defined to be the coefficient of the x^31 term.
computeCRC :: B.ByteString -> Word32
computeCRC bytes = 0  -- FIXME: implement or re-use a proper CRC function
