module Set1.Challenge5.Tests (c5Tree) where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge5

c5Tree :: TestTree
c5Tree = testGroup "Challenge 5" [challenge5Unit]

challenge5Unit :: TestTree
challenge5Unit = testGroup "Challenge 5 unit tests" [
  testCase "Cryptopals example" challenge5]

challenge5 :: Assertion
challenge5 =
  (C.pack "b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
  @=?
  (repeatingKeyXor
   (C.pack "ICE")
   (C.pack "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"))
