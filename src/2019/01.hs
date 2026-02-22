{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Arrow ((&&&))

main :: IO ()
main = print . solve . map read . lines =<< readFile "input/2019/01.txt"

-- >>> solve [1969]
-- (654,966)
solve :: [Int] -> (Int, Int)
solve = (sum . map fuel) &&& (sum . map totalFuel)

fuel, totalFuel :: Int -> Int
fuel x = x `div` 3 - 2
totalFuel = sum . takeWhile (> 0) . tail . iterate fuel
