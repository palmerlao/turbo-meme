module Set1.Challenge2 where

import Data.Bits (xor)
import qualified Data.ByteString.Char8 as C

import Set1.Challenge1 (int2hex, hex2int, Encoded, Hex)

-- challenge2: xor two equal-length hex strings
fixedXor :: Encoded Hex -> Encoded Hex -> Encoded Hex
fixedXor x y = int2hex $ (hex2int x) `xor` (hex2int y)
