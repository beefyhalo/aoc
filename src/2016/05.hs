{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Crypto.Hash (Digest, MD5, hash)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as M
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Word (Word8)

main :: IO ()
main = do
  input <- BC.readFile "input/2016/05.txt"
  putStrLn $ solve input
  putStrLn $ partTwo input

-- >>> solve "abc"
-- >>> partTwo "abc"
-- "18f47a30"
-- "05ace8e3"
solve, partTwo :: BC.ByteString -> String
solve key = take 8 [hex (BA.index d 2) | n <- [0 ..], let d = getHash key n, isValid d]
partTwo key = M.elems $ fromJust $ find ((== 8) . M.size) $ scanl step M.empty [0 ..]
  where
    step :: M.IntMap Char -> Int -> M.IntMap Char
    step !m n
      | isValid d && pos < 8 && M.notMember pos m = M.insert pos val m
      | otherwise = m
      where
        d = getHash key n
        pos = fromIntegral (BA.index d 2)
        val = hex (BA.index d 3 `shiftR` 4)

getHash :: BC.ByteString -> Int -> Digest MD5
getHash key n = hash (key <> BC.pack (show n))

isValid :: Digest MD5 -> Bool
isValid d = BA.index d 0 == 0 && BA.index d 1 == 0 && BA.index d 2 .&. 0xF0 == 0

hex :: Word8 -> Char
hex n = "0123456789abcdef" !! fromIntegral n
