module Set1.Challenge2 (
  fixedXor
  ) where

import Data.Bits (xor)
import qualified Data.ByteString.Char8 as C

import Set1.Challenge1 (int2hex, hex2int)

-- challenge2: xor two equal-length hex strings
fixedXor :: C.ByteString -> C.ByteString -> C.ByteString
fixedXor x y = int2hex $ (hex2int x) `xor` (hex2int y)
