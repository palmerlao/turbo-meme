module Set1.Challenge6.Tests (c6Tree) where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge6 

c6Tree :: TestTree
c6Tree = testGroup "Challenge 6" [challenge6Unit]

challenge6Unit :: TestTree
challenge6Unit = testGroup "Challenge 6" [
  testCase "Cryptopals example" challenge6]

challenge6 :: Assertion
challenge6 =
  (37::Int) @=? hammingDist (C.pack "this is a test") (C.pack "wokka wokka!!!")
