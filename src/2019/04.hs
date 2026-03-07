{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (group)
import Data.List.Split (splitOn)

digits :: Int -> [Int]
digits 0 = []
digits x = let (q, r) = x `divMod` 10 in digits q ++ [r]

main :: IO ()
main = print . solve . map read . splitOn "-" =<< readFile "input/2019/04.txt"

-- >>> solve [111111, 111122]
-- (10,1)
solve :: [Int] -> (Int, Int)
solve [lo, hi] = (length $ filter valid1 nums, length $ filter valid2 nums)
  where
    nums = [lo .. hi]

    valid1 n = let ds = digits n in nonDec ds && any ((>= 2) . length) (group ds)
    valid2 n = let ds = digits n in nonDec ds && any ((== 2) . length) (group ds)

    nonDec xs = and $ zipWith (<=) xs (tail xs)
