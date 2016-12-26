module Set1.Tests (
  challenge1,
  challenge2
  )
       where

import Test.HUnit
import qualified Data.ByteString.Char8 as C

import Set1.Challenge1
import Set1.Challenge2


challenge1 :: Assertion
challenge1 = assertEqual "hex2base64 example"
  (C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  (hex2base64 . C.pack $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

challenge2 :: Assertion
challenge2 = assertEqual "fixed xor example"
  (C.pack "746865206b696420646f6e277420706c6179")
  (fixedXor
   (C.pack "1c0111001f010100061a024b53535009181c")
   (C.pack "686974207468652062756c6c277320657965"))
