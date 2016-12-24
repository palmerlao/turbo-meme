module Set1.Tests (
  cryptopals1  
  )
       where

import Test.HUnit
import qualified Data.ByteString.Char8 as C

import Set1 (hex2base64)

cryptopals1 :: Assertion
cryptopals1 = assertEqual "hex2base64 example from website"
  (hex2base64 . C.pack $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  (C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
