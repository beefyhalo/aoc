{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Hash (Digest, MD5, hash)
import Data.Bits ((.&.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  input <- B.readFile "input/2015/04.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve ["abcdef", "pqrstuv"]
-- [609043,1048970]
solve, partTwo :: B.ByteString -> Int
solve key = head [n | n <- [1 ..], hasFiveZeros (key <> BC.pack (show n))]
-- >>> map partTwo ["abcdef", "pqrstuv"]
-- [6742839,5714438]
partTwo key = head [n | n <- [1 ..], hasSixZeros (key <> BC.pack (show n))]

-- >>> hash (BC.pack "abcdef609043") :: Digest MD5
-- 000001dbbfa3a5c83a2d506429c7b00e
hasFiveZeros :: B.ByteString -> Bool
hasFiveZeros bs = b0 == 0 && b1 == 0 && b2 .&. 0xF0 == 0 -- first 16 bits zero and high nibble of 3rd byte zero
  where
    digest = hash bs :: Digest MD5
    b0 = BA.index digest 0
    b1 = BA.index digest 1
    b2 = BA.index digest 2

hasSixZeros :: B.ByteString -> Bool
hasSixZeros bs = b0 == 0 && b1 == 0 && b2 == 0
  where
    digest = hash bs :: Digest MD5
    b0 = BA.index digest 0
    b1 = BA.index digest 1
    b2 = BA.index digest 2
