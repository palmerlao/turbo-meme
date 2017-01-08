module Set1.Challenge1 where

import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char (chr)
import Data.List (foldl', unfoldr)
import Data.Map (fromList, (!))

import Debug.Trace

import GHC.Exts (IsString)

data Base64
data Hex
newtype Encoded a = Encoded {unwrap :: C.ByteString}
                  deriving (Show)

checkValidChars :: C.ByteString -> [Char] -> Encoded a
checkValidChars str cs = if C.all (`elem` cs) str
                         then Encoded str
                         else error $
                              "found invalid characters: " ++
                              (C.unpack $ C.filter (not . (`elem` cs)) str)

base64 :: C.ByteString -> Encoded Base64
base64 = (`checkValidChars` b64Syms)

hex :: C.ByteString -> Encoded Hex
hex = (`checkValidChars` hexSyms)

-- challenge 1: convert hex to base64
bin2base :: Int -> String -> Integer -> C.ByteString
bin2base bits syms =
  let upper = 2^bits-1
      i2c = fromList $ zip [0..upper] syms
  in  C.reverse .
      C.unfoldr (\x -> if x > 0
                       then Just (i2c ! (x .&. upper), x `shiftR` bits)
                       else Nothing)

base2bin :: Int -> String -> C.ByteString -> Integer
base2bin bits syms =
  let upper = 2^bits-1
      c2i = fromList $ syms `zip` [0..upper]
  in  C.foldl' (\i c -> (upper+1)*i + (c2i ! c)) 0

b64Syms = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/', '=']
hexSyms = ['0'..'9'] ++ ['a'..'f']

int2c = bin2base 8 $ fmap chr [0..255]
c2int = base2bin 8 $ fmap chr [0..255]

int2hex :: Integer -> Encoded Hex
int2hex = hex . bin2base 4 hexSyms

hex2int :: Encoded Hex -> Integer
hex2int (Encoded str) = base2bin 4 hexSyms str
hex2base64 :: Encoded Hex -> Encoded Base64
hex2base64 = int2base64 . hex2int

base642c = int2c . base642int
c2base64 = int2base64 . c2int
c2hex = int2hex . c2int
hex2c = int2c .  hex2int


chunkAndPad :: Int -> Char -> C.ByteString -> [C.ByteString]
chunkAndPad sz pad str =
  let chunkHelper str =
        case C.splitAt sz str of
          (h, t) | C.length h == sz -> Just (h, t)
          (h, t) | C.length h == 0 -> Nothing
          (h, t) -> Just (h `C.append` (C.replicate (sz - (C.length h)) pad), t)
  in  unfoldr chunkHelper str

chunk :: Int -> C.ByteString -> [C.ByteString]
chunk sz str =
  let chunkHelper str =
        case C.splitAt sz str of
          (h, t) | C.length h == sz -> Just (h, t)
          (h, t) | C.length h == 0 -> Nothing
  in  unfoldr chunkHelper str

-- probably slightly slower than optimal
numBytes :: Integer -> Int
numBytes i =
  let numBits = floor . logBase 2.0 . fromIntegral $ i
  in  head $ filter (\k -> k*8 >= numBits) [0..]

int2base64 :: Integer -> Encoded Base64
int2base64 i =
  let nb = traceShowId $ numBytes i 
      numPadBytes = traceShowId $ (3 - (nb `mod` 3)) `mod` 3
      padded = i `shiftL` (8*numPadBytes)
      decoded = traceShowId $ bin2base 6 b64Syms padded 
  in  base64 $ (C.take (C.length decoded-numPadBytes) decoded) `C.append` (C.replicate numPadBytes '=')

paddedBase642int :: C.ByteString -> (Integer, Int)
paddedBase642int str = 
  let nBytes      = 3 - (C.count '=' str)
      decoded     = base2bin 6 b64Syms $ C.map (\c -> if c=='=' then 'A' else c) str
      shift       = 8*(3 - nBytes)
  in  (decoded `shiftR` shift, 8*nBytes)

base642int :: Encoded Base64 -> Integer
base642int (Encoded str) =
  let chunks = chunk 4 str
      initNum =
        sum $ zipWith
        (\a b -> a * 64^b)
        (fmap (base2bin 6 b64Syms) (init chunks))
        [0..]
      (lastNum, nBits) = paddedBase642int $ last chunks
  in  (initNum `shiftL` nBits) + lastNum
