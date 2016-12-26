import Test.Tasty
import Test.Tasty.HUnit

import Set1.Challenge1.Tests
import Set1.Challenge2.Tests
import Set1.Challenge5.Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  c1Tree,
  c2Tree,
  c5Tree]
