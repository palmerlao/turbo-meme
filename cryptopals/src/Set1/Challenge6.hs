module Set1.Challenge6 where

import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Foldable (traverse_)
import Data.List (unfoldr)
import Data.Set

import GHC.Exts (sortWith)

import System.IO (openFile, IOMode(ReadMode))

import Set1.Challenge1       
import Set1.Challenge3

hammingDist :: C.ByteString -> C.ByteString -> Int
hammingDist s1 s2 = 
  popCount $ (c2int s1) `xor` (c2int s2)

keysizeDist :: C.ByteString -> [(Int, Double)]
keysizeDist str =
  [ (x,
     ((fromIntegral $
       hammingDist
       (C.take x str)
       (C.take x $ C.drop x str)))/
     (fromIntegral x)) | x <- [2..40]]

readCipherText :: IO (Encoded Base64)
readCipherText = do
  secretsFile <- openFile "data/Set1/6.txt" ReadMode
  cipherLined <- C.hGetContents $ secretsFile
  return . base64 . C.concat . C.lines $ cipherLined

rankKeySizeGuesses :: Encoded Base64 -> [Int]
rankKeySizeGuesses secrets = fmap fst $ sortWith snd $ keysizeDist (base642c secrets)

chunkAndPad :: Int -> Char -> C.ByteString -> [C.ByteString]
chunkAndPad sz pad str =
  let chunkHelper str =
        case C.splitAt sz str of
          (h, t) | C.length h == sz -> Just (h, t)
          (h, t) | C.length h == 0 -> Nothing
          (h, t) -> Just (h `C.append` (C.replicate (sz - (C.length h)) pad), t)
  in  unfoldr chunkHelper str


decodeRepeatedXor :: Int -> Encoded Hex -> Encoded Hex
decodeRepeatedXor sz str =
  let fst (a,b,c) = a
      thd (a,b,c) = c
      chunks = chunkAndPad sz '0' $ unwrap str
      chunkst = C.transpose chunks
      allDecodings :: Encoded Hex -> [(Encoded Hex, C.ByteString, Double)]
      allDecodings cipher =
        fmap (\k -> let plain = hex2c $ decode k cipher in (k, plain, score plain)) hexKeys
      bestKey :: Encoded Hex -> (Encoded Hex, C.ByteString, Double)
      bestKey chunk = head $ sortWith thd $ allDecodings chunk
  in  hex $ C.concat $ fmap (\c -> unwrap $ fst $ bestKey c) $ fmap hex chunkst

solution = do
  secrets <- readCipherText
  let candidateKeySizes = rankKeySizeGuesses secrets
      hexCipher = int2hex . base642int $ secrets
      keyList = fmap (\sz -> decodeRepeatedXor sz hexCipher) $ take 6 candidateKeySizes
      decoded = fmap hex2c $ zipWith decode keyList (repeat hexCipher)
  putStrLn . unlines . (fmap show) $ keyList
  C.putStrLn $ C.unlines decoded
{--}
