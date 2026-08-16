{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Crypto.Hash.MD5 (hash)
import Data.Bits ((.&.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Word (Word8)

main :: IO ()
main = do
  input <- B.readFile "input/2015/04.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve ["abcdef", "pqrstuv"]
-- >>> map partTwo ["abcdef", "pqrstuv"]
-- [609043,1048970]
-- [6742839,5714438]
solve, partTwo :: B.ByteString -> Int
solve key = head [n | n <- [1 ..], hasFiveZeros (key <> BC.pack (show n))]
partTwo key = head [n | n <- [1 ..], hasSixZeros (key <> BC.pack (show n))]

hasFiveZeros :: B.ByteString -> Bool
hasFiveZeros bs = b0 == 0 && b1 == 0 && b2 .&. 0xF0 == 0 -- first 16 bits zero and high nibble of 3rd byte zero
  where
    (b0, b1, b2) = bytes bs

hasSixZeros :: B.ByteString -> Bool
hasSixZeros bs = bytes bs == (0, 0, 0)

bytes :: B.ByteString -> (Word8, Word8, Word8)
bytes bs = (b0, b1, b2)
  where
    digest = hash bs
    b0 = BA.index digest 0
    b1 = BA.index digest 1
    b2 = BA.index digest 2
