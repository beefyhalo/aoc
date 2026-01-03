{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Crypto.Hash (MD5, hash)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  input <- B.readFile "input/2016/14.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve "abc"
-- >>> partTwo "abc"
-- 22728
-- 22551
solve, partTwo :: B.ByteString -> Int
solve salt = findKeys salt 0 !! 63
partTwo salt = findKeys salt 2016 !! 63

findKeys :: B.ByteString -> Int -> [Int]
findKeys salt n =
  [ i
  | (i, h) <- zip [0 ..] hashes,
    Just c <- [firstTriplet h],
    any (hasQuintuplet c) (take 1000 $ drop (i + 1) hashes)
  ]
  where
    hashes = [iterate md5 (md5 (salt <> B.pack (show i))) !! n | i <- [0 ..]]

md5 :: B.ByteString -> B.ByteString
md5 = convertToBase Base16 . hash @_ @MD5

firstTriplet :: B.ByteString -> Maybe Char
firstTriplet s = listToMaybe [B.head g | g <- B.group s, B.length g >= 3]

hasQuintuplet :: Char -> B.ByteString -> Bool
hasQuintuplet = B.isInfixOf . B.replicate 5
