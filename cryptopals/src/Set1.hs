module Set1 (
  test,
  testShort
  )
       where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.Int (Int8)
import Data.Map (fromList, (!))

test = C.pack "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

testShort = C.pack "49276d2"

intbase64 = fromList $ zip ([0..63]::[Integer]) $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']
hexint = fromList $ zip (['0'..'9'] ++ ['a'..'f']) [0..15]

hex2int :: BS.ByteString -> Integer
hex2int = C.foldl' (\i c -> 16*i + (hexint ! c)) 0 

int2base64 :: Integer -> BS.ByteString
int2base64 =
  BS.reverse .
  C.unfoldr (\x ->
              if x > 0
              then Just (intbase64 ! (x .&. 63), shiftR x 6)
              else Nothing)

hex2base64 = int2base64 . hex2int
