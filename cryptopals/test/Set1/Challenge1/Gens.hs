module Set1.Challenge1.Gens where

import qualified Data.ByteString.Char8 as C

import Test.QuickCheck
import Set1.Challenge1

genNumBits :: Gen Int
genNumBits = choose (1::Int, 8)

genBitsSyms :: Gen (Int, String)
genBitsSyms = do
  nbits <- genNumBits
  return (nbits, take (2^nbits) ['\0'..'\255'])

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

genBase64String :: Gen C.ByteString
genBase64String = fmap C.pack $ listOf . oneof $ fmap return b64Syms

chunkGens :: Gen (Int, C.ByteString)
chunkGens = do
  str <- genBase64String
  i <- choose (1, 1 `max` ((C.length str) - 1))
  return (i , str)
