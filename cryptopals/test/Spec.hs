import Test.Tasty
import Test.Tasty.HUnit

import Set1.Tests (challenge1, challenge2)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
  testCase "hex2base64 example" challenge1,
  testCase "fixed xor example" challenge2]
