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
  testCase "Cryptopals example" challenge1,
  testCase "base642c TQ== is M" mExample,
  testCase "base642c TWE= is Ma" maExample,
  testCase "base642c TWFu is Man" manExample,
  testCase "c2base64 M is TQ==" mExample',
  testCase "c2base64 Ma is TWE=" maExample',
  testCase "c2base64 Man is TWFu" manExample',
  testCase "pleasure. wiki example" pleasureExample,
  testCase "leasure. wiki example" leasureExample,
  testCase "easure. wiki example" easureExample
  ]

challenge1 :: Assertion
challenge1 = 
  (C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  @=?
  (hex2base64 . C.pack $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

mExample :: Assertion
mExample =
  (C.pack "M") @=? (base642c $ C.pack "TQ==")

maExample :: Assertion
maExample =
  (C.pack "Ma") @=? (base642c $ C.pack "TWE=")

manExample :: Assertion
manExample =
  (C.pack "Man") @=? (base642c $ C.pack "TWFu")
  
mExample' :: Assertion
mExample' =
  (C.pack "TQ==") @=? (c2base64 $ C.pack "M")

maExample' :: Assertion
maExample' =
  (C.pack "TWE=") @=? (c2base64 $ C.pack "Ma")

manExample' :: Assertion
manExample' =
  (C.pack "TWFu") @=? (c2base64 $ C.pack "Man")

pleasureExample :: Assertion
pleasureExample =
  (C.pack "cGxlYXN1cmUu") @=? (c2base64 $ C.pack "pleasure.")

leasureExample :: Assertion
leasureExample =
  (C.pack "bGVhc3VyZS4=") @=? (c2base64 $ C.pack "leasure.")

easureExample :: Assertion
easureExample =
  (C.pack "ZWFzdXJlLg==") @=? (c2base64 $ C.pack "easure.")

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
    let chunked = chunkAndPad sz '\0' str
    in  (C.length . C.concat $ chunked)
        ==
        (sz*(length chunked))
  )

bin2bin :: Int -> String -> Integer -> Integer
bin2bin bits syms =  (base2bin bits syms) . (bin2base bits syms)

base642base64 :: C.ByteString -> C.ByteString
base642base64 = int2Base64 . base642int

int2int :: Integer -> Integer
int2int = base642int . int2Base64

base2base :: Int -> String -> C.ByteString -> C.ByteString
base2base bits syms = (bin2base bits syms) . (base2bin bits syms)
