module Set1.Challenge1 (
  int2hex,
  hex2int,
  hexSyms,
  base2bin,
  bin2base,
  hex2base64
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Map (fromList, (!))

-- challenge 1: convert hex to base64
bin2base :: Int -> String -> Integer -> C.ByteString
bin2base bits syms =
  let upper = 2^bits-1
      i2c = fromList $ zip [0..upper] syms
  in  BS.reverse .
      C.unfoldr (\x -> if x > 0
                       then Just (i2c ! (x .&. upper), x `shiftR` bits)
                       else Nothing)
base2bin :: Int -> String -> C.ByteString -> Integer
base2bin bits syms =
  let upper = 2^bits-1
      c2i = fromList $ syms `zip` [0..upper]
  in  C.foldl' (\i c -> (upper+1)*i + (c2i ! c)) 0

b64Syms = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']
hexSyms = ['0'..'9'] ++ ['a'..'f']

int2base64 = bin2base 6 b64Syms
int2hex = bin2base 4 hexSyms

hex2int = base2bin 4 hexSyms
hex2base64 = int2base64 . hex2int