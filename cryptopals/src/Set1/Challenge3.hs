module Set1.Challenge3 where

import qualified Data.ByteString.Char8 as C
import Data.Char (chr, toLower)
import Data.Coerce (coerce)
import Data.List (foldl')

import GHC.Exts (sortWith)

import Set1.Challenge1
import Set1.Challenge2

-- challenge3: decode a hex string that was xor'd with one character
encoded = hex $ C.pack "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
intEncoded = hex2int encoded
hexKeys = [ hex $ C.pack [x,y] | x <- hexSyms, y <- hexSyms ]

decode :: Encoded Hex -> Encoded Hex -> Encoded Hex
decode key str =
  let keyBuf :: Encoded Hex
      keyBuf = hex $ C.concat . (fmap unwrap) $ take ((C.length . unwrap $ encoded) `div` (C.length . unwrap $ key)) $ repeat key
  in  fixedXor keyBuf str

normalize :: [(Char, Double)] -> [(Char, Double)]
normalize counts =
  let norm = foldl' (+) 0.0 $ fmap snd counts
  in  if norm > 0
      then fmap (\(c,f) -> (c, f/norm)) counts
      else counts

englishFreqs = normalize $ ['a'..'z'] `zip` [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.07]

dist f1 f2 = foldl' (+) 0.0 $ [abs $ s1-s2 | (c1,s1) <- f1, (c2,s2) <- f2, c1==c2]

score :: C.ByteString -> Double
score str =
  let freq = normalize $ fmap (\c -> (c, fromIntegral $ C.count c $ C.map toLower str)) ['a'..'z']
  in  dist freq englishFreqs

allDecodings :: Encoded Hex -> [Encoded Hex]
allDecodings secret = fmap (\k -> decode k secret) hexKeys

topDecodings :: Encoded Hex -> Int -> [C.ByteString]
topDecodings secret k =
  let messages = fmap hex2c $ allDecodings secret
  in  take k $ sortWith score messages

solution = do
  putStrLn "Top 5 decodings for Set 1, Challenge 3:"
  C.putStrLn $ C.unlines $ topDecodings encoded 5
