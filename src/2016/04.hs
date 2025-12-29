{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Data.Bifunctor (second)
import Data.Char (ord)
import qualified Data.IntMap.Monoidal.Strict as MM
import Data.List (find, isInfixOf, sortOn, unsnoc)
import Data.List.Split (splitOn)
import Data.Monoid (Sum)
import Data.Ord (Down (..))
import Data.Tuple (swap)
import GHC.Char (chr)

data Room = Room {name :: String, sectorId :: Int, checksum :: String}

-- $setup
-- >>> input = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/04.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Room
parse s = Room (concat nameParts) (read sid) checksum
  where
    Just (nameParts, lastPart) = unsnoc $ splitOn "-" s
    (sid, chkStr) = span (/= '[') lastPart
    checksum = filter (/= ']') $ drop 1 chkStr

-- >>> solve example
-- 1514
solve, partTwo :: [Room] -> Int
solve = sum . map sectorId . filter isReal
partTwo = maybe 0 sectorId . find (isInfixOf "northpole" . decrypt)

freq :: String -> MM.MonoidalIntMap (Sum Int)
freq = foldMap ((`MM.singleton` 1) . ord)

computeChecksum :: String -> String
computeChecksum = take 5 . map (chr . fst) . sortOn (swap . second Down) . MM.toList . freq

isReal :: Room -> Bool
isReal r = computeChecksum (name r) == checksum r

-- >>> map decrypt example
-- ["tttttuuusrq","zabcdefg","bchofsozfcca","lglsddqjwsdjgge"]
decrypt :: Room -> String
decrypt r = shiftChar (sectorId r) <$> name r

shiftChar :: Int -> Char -> Char
shiftChar n k = chr $ (ord k - ord 'a' + n) `mod` 26 + ord 'a'
