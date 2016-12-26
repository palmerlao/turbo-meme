module Set1.Challenge2.Tests where

import qualified Data.ByteString.Char8 as C

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Set1.Challenge1 (hexSyms)
import Set1.Challenge2

c2Tree :: TestTree
c2Tree = testGroup "Challenge 2" [challenge2Unit, challenge2Props]

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

challenge2Props :: TestTree
challenge2Props = testGroup "Challenge 2 properties" [
  testProperty "fixedXor is idempotent" prop_xorIdempotence,
  testProperty "fixedXor is commutative" prop_xorCommutative,
  testProperty "fixedXor is associative" prop_xorAssociative
  ]

gen2Hex :: Gen (C.ByteString, C.ByteString)
gen2Hex = gen3Hex >>= (\(a,b,c) -> return (a,b))

gen3Hex :: Gen (C.ByteString, C.ByteString, C.ByteString)
gen3Hex = do
  n <- arbitrarySizedNatural
  h1 <- vectorOf n $ oneof $ fmap return hexSyms
  h2 <- vectorOf n $ oneof $ fmap return hexSyms
  h3 <- vectorOf n $ oneof $ fmap return hexSyms
  return (C.pack h1, C.pack h2, C.pack h3)

-- drops leading zeros
prop_xorIdempotence =
  forAll
  gen2Hex
  (\(a, b) -> a `fixedXor` b `fixedXor` b == C.dropWhile (==head hexSyms) a)

prop_xorCommutative =
  forAll
  gen2Hex
  (\(a, b) -> a `fixedXor` b == b `fixedXor` a)

prop_xorAssociative =
  forAll
  gen3Hex
  (\(a, b, c) -> ((a `fixedXor` b) `fixedXor` c) == (a `fixedXor` (b `fixedXor` c)))
