module Set1.Challenge1.Tests (c1Tree) where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge1

c1Tree :: TestTree
c1Tree = testGroup "Challenge 1" [challenge1Unit, challenge1Props]

challenge1Unit :: TestTree
challenge1Unit = testGroup "Challenge 1 unit tests" [
  testCase "Cryptopals example" challenge1]

challenge1 :: Assertion
challenge1 = 
  (C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  @=?
  (hex2base64 . C.pack $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

challenge1Props :: TestTree
challenge1Props = testGroup "Challenge 1 properties" [
  testProperty "bin2base . base2bin == id" prop_id1,
  testProperty "base2bin . bin2base == id" prop_id2
  ]

genBitsSyms :: Gen (Int, String)
genBitsSyms = do
  bits <- choose (1::Int, 8)
  let syms = take (2^bits) ['\0'..'\255']
  return (bits, syms)

gen1 :: Gen (Int, String, Integer)
gen1 = do
  (bits, syms) <- genBitsSyms
  i <- arbitrarySizedNatural
  return (bits, syms, i)

-- drops leading zeros
gen2 :: Gen (Int, String, C.ByteString)
gen2 = do
  (bits, syms) <- genBitsSyms
  str <- listOf . oneof $ fmap return syms
  return (bits, syms, C.pack $ dropWhile (== head syms) str)

prop_id1 = forAll gen1 (\(bits, syms, int) -> bin2bin bits syms int == int)

prop_id2 = forAll gen2 (\(bits, syms, str) -> base2base bits syms str == str)

bin2bin :: Int -> String -> Integer -> Integer
bin2bin bits syms =  (base2bin bits syms) . (bin2base bits syms)

base2base :: Int -> String -> C.ByteString -> C.ByteString
base2base bits syms = (bin2base bits syms) . (base2bin bits syms)
