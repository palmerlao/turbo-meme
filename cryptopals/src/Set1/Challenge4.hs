module Set1.Challenge4 where

import qualified Data.ByteString.Char8 as C

import GHC.Exts (sortWith)

import System.IO (openFile, IOMode(ReadMode))

import Set1.Challenge3 (score, topDecodings)

someLetters :: C.ByteString -> Bool
someLetters s =
  let nLetters = C.length $ C.filter (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'])) s
      readable = C.length $ C.filter (\c -> c `elem` ['\32'..'\128']) s
  in  nLetters > 10 && readable >= 17

substr :: C.ByteString -> C.ByteString -> Bool
substr sub str = case C.breakSubstring sub str of
  (x,y) | C.null y    -> False
        | otherwise   -> True

-- combing through some preliminary results shows a string along the lines of
-- nOWTHATTHEPARTYISJUMPING*
findEncoded :: [C.ByteString] -> [C.ByteString]
findEncoded strs =
  fmap (\(a,b) -> a `C.append` (C.pack "\t") `C.append` b) $
  filter (\(e,d) -> substr (C.pack "PARTY") d)
  (concat $ fmap (\x -> (repeat x) `zip` (topDecodings x 10)) strs)

-- which corresponds to this hex string in the file
secret = C.pack "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"

solution = do
  putStrLn "Top 5 decodings for Set 1, Challenge 4:"
  C.putStrLn $ C.unlines $ topDecodings secret 5

{-
solution = do
  secretsFile <- openFile "data/Set1/4.txt" ReadMode
  secrets <- fmap C.lines $ C.hGetContents $ secretsFile
  putStrLn $ "Top scoring decodes for Set 1, Challenge 4:"
  C.putStrLn $ C.unlines $ findEncoded secrets
-}
