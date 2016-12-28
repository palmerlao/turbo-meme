module Set1.Challenge1 where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (chr)
import Data.List (foldl', unfoldr)
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

int2hex = bin2base 4 hexSyms

hex2int = base2bin 4 hexSyms
hex2base64 = int2base64 . hex2int
hex2c = (bin2base 8 $ fmap chr [0..255]) .  (base2bin 4 hexSyms)
c2hex = (bin2base 4 hexSyms) . (base2bin 8 $ fmap chr [0..255])

c2int = base2bin 8 $ fmap chr [0..255]
int2c = bin2base 8 $ fmap chr [0..255]

chunk :: Int -> Char -> C.ByteString -> [C.ByteString]
chunk sz pad str =
  let chunkHelper str =
        case C.splitAt sz str of
          (h, t) | C.length h == sz -> Just (h, t)
          (h, t) | C.length h == 0 -> Nothing
          (h, t) -> Just (h `C.append` (C.replicate (sz - (C.length h)) pad), t)
  in  unfoldr chunkHelper str

int2base64 :: Integer -> C.ByteString
int2base64 i = undefined

base642int :: C.ByteString -> Integer
base642int str =
  let chunks = chunk 3 '=' str
  in  undefined
