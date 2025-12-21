{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (transpose)
import Data.List.Split (wordsBy)

type Ingredient = [Int] -- [capacity, durability, flavor, texture, calories]

-- $setup
-- >>> input = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2015/15.txt"
  print $ solve Nothing input
  print $ solve (Just 500) input

parse :: String -> Ingredient
parse s = case map read $ wordsBy (`notElem` "-0123456789") s of
  [c, d, f, t, a] -> [c, d, f, t, a]

-- >>> solve Nothing example
-- >>> solve (Just 500) example
-- 62842880
-- 57600000
solve :: Maybe Int -> [Ingredient] -> Int
solve mCal ingreds =
  maximum
    [ score amounts ingreds
      | amounts <- splits (length ingreds) 100,
        maybe True (== calories amounts ingreds) mCal
    ]
  where
    calories amounts = sum . zipWith (*) amounts . map last

    score :: [Int] -> [Ingredient] -> Int
    score amounts = product . map (max 0 . sum) . transpose . zipWith (map . (*)) amounts . map init

    splits :: Int -> Int -> [[Int]]
    splits 1 total = [[total]]
    splits n total = [x : xs | x <- [0 .. total], xs <- splits (n - 1) (total - x)]
