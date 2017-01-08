module Set1.Challenge5 where

import qualified Data.ByteString.Char8 as C

import Set1.Challenge1 (hex2c, c2hex, Encoded, Hex, hex, unwrap)
import Set1.Challenge2 (fixedXor)

repeatingKeyXor :: C.ByteString -> C.ByteString -> Encoded Hex
repeatingKeyXor key plaintext =
  let n       = C.length plaintext
      longKey = C.take n $ C.concat $ take n $ repeat key
      hPlain  = c2hex plaintext
      hKey    = c2hex longKey
  in  hPlain `fixedXor` hKey
