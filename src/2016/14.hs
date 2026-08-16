import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Crypto.Hash.MD5 (hash)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString.Char8 qualified as B
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
solve = (!! 63) . findKeys 0
partTwo = (!! 63) . findKeys 2016

findKeys :: Int -> B.ByteString -> [Int]
findKeys n salt =
  [ i
  | (i, h) <- zip [0 ..] hashes,
    Just c <- [firstTriplet h],
    any (hasQuintuplet c) (take 1000 $ drop (i + 1) hashes)
  ]
  where
    hashes = [stretch n (md5 (salt <> B.pack (show i))) | i <- [0 :: Int ..]] `using` parBuffer 512 rdeepseq

md5 :: B.ByteString -> B.ByteString
md5 = convertToBase Base16 . hash

stretch :: Int -> B.ByteString -> B.ByteString
stretch n = (!! n) . iterate md5

firstTriplet :: B.ByteString -> Maybe Char
firstTriplet s = listToMaybe [B.head g | g <- B.group s, B.length g >= 3]

hasQuintuplet :: Char -> B.ByteString -> Bool
hasQuintuplet = B.isInfixOf . B.replicate 5
