import Test.Tasty
import Test.Tasty.HUnit

import Set1.Challenge1.Tests
import Set1.Challenge2.Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  unitTests,
  properties]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
  challenge1Unit,
  challenge2Unit]

properties :: TestTree
properties = testGroup "Properties" [
  challenge1QC]
