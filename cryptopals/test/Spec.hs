import Test.Tasty
import Test.Tasty.HUnit

import Set1.Tests (cryptopals1)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testCase "hex2base64 example from website" cryptopals1]
