module Set1.Challenge1.Tests (c1Tree, prop_chunk) where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge1

import Set1.Challenge1.Gens

c1Tree :: TestTree
c1Tree = testGroup "Challenge 1" [challenge1Unit, challenge1Props]

challenge1Unit :: TestTree
challenge1Unit = testGroup "Challenge 1 unit tests" [
  testCase "Cryptopals example" challenge1
  ]


challenge1 :: Assertion
challenge1 = 
  (C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  @=?
  (hex2base64 . C.pack $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

challenge1Props :: TestTree
challenge1Props = testGroup "Challenge 1 properties" [
  testProperty "bin2base . base2bin == id" prop_id1
  , testProperty "base2bin . bin2base == id" prop_id2
  , testProperty "length . concat $ chunked == sz*(length chunked)" prop_chunk
--  , testProperty "base642int . int2base64 == id" prop_id3
--  , testProperty "int2base64 . base642int == id" prop_id4
  ]

prop_id1 = forAll gen1 (\(bits, syms, int) -> bin2bin bits syms int == int)

prop_id2 = forAll gen2 (\(bits, syms, str) -> base2base bits syms str == str)

prop_id3 = forAll genBase64String (\str -> base642base64 str == str)
prop_id4 i = int2int i == i

prop_chunk =
  forAll
  chunkGens
  (\(sz, str) ->
    let chunked = chunk sz '\0' str
    in  (C.length . C.concat $ chunked)
        ==
        (sz*(length chunked))
  )

bin2bin :: Int -> String -> Integer -> Integer
bin2bin bits syms =  (base2bin bits syms) . (bin2base bits syms)

base642base64 :: C.ByteString -> C.ByteString
base642base64 = int2base64 . base642int

int2int :: Integer -> Integer
int2int = base642int . int2base64

base2base :: Int -> String -> C.ByteString -> C.ByteString
base2base bits syms = (bin2base bits syms) . (base2bin bits syms)
