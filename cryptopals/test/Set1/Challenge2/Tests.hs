module Set1.Challenge2.Tests where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge2

challenge2Unit :: TestTree
challenge2Unit = testGroup "Challenge 2 unit tests" [
  testCase "Cryptopals example" challenge2]


challenge2 :: Assertion
challenge2 =
  (C.pack "746865206b696420646f6e277420706c6179")
  @=?
  (fixedXor
   (C.pack "1c0111001f010100061a024b53535009181c")
   (C.pack "686974207468652062756c6c277320657965"))
