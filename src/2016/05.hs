{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens (ix, none, (&), (.~))
import Control.Parallel.Strategies (parBuffer, rseq, using)
import Crypto.Hash (Digest, MD5, hash)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BC
import Data.List (find, scanl')
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Text.Printf (printf)

-- $setup
-- >>> example = candidates "abc"

main :: IO ()
main = do
  input <- candidates <$> BC.readFile "input/2016/05.txt"
  putStrLn $ solve input
  putStrLn $ partTwo input

-- >>> solve example
-- >>> partTwo example
-- "18f47a30"
-- "05ace8e3"
solve, partTwo :: [Digest MD5] -> String
solve keys = take 8 [hex (BA.index d 2) | d <- keys]
partTwo keys = head [h | h <- scanl' step (replicate 8 '_') keys, none (== '_') h]
-- partTwo keys = fromJust . find (none (== '_')) . scanl' step (replicate 8 '_')
  where
    step :: String -> Digest MD5 -> String
    step out d
      | pos < 8 && out !! pos == '_' = out & ix pos .~ val
      | otherwise = out
      where
        pos = fromIntegral (BA.index d 2)
        val = hex (BA.index d 3 `shiftR` 4)

getHash :: BC.ByteString -> Int -> Digest MD5
getHash key n = hash (key <> BC.pack (show n))

isValid :: Digest MD5 -> Bool
isValid d = BA.index d 0 == 0 && BA.index d 1 == 0 && BA.index d 2 .&. 0xF0 == 0

hex :: Word8 -> Char
hex = head . printf "%x"

-- Generate infinite list of valid hashes in parallel chunks
candidates :: BC.ByteString -> [Digest MD5]
candidates key = concatMap mine indexChunks `using` parBuffer parBuff rseq
  where
    chunkSize = 100_000
    parBuff = 16
    indexChunks = chunksOf chunkSize [0 ..]
    mine ns = [d | n <- ns, let d = getHash key n, isValid d]
