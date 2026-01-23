{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Move = Spin Int | Exchange Int Int | Partner Char Char

-- $setup
-- >>> input = "s1,x3/4,pe/b"
-- >>> example = map parse (splitOn "," input)

main :: IO ()
main = do
  input <- map parse . splitOn "," <$> readFile "input/2017/16.txt"
  let ps = V.fromList ['a' .. 'p']
  putStrLn $ V.toList $ solve ps input
  putStrLn $ V.toList $ partTwo ps input

parse :: String -> Move
parse s
  | Just rest <- stripPrefix "s" s = Spin (read rest)
  | Just rest <- stripPrefix "x" s = let [a, b] = splitOn "/" rest in Exchange (read a) (read b)
  | Just rest <- stripPrefix "p" s = let [a : _, b : _] = splitOn "/" rest in Partner a b

-- >>> solve (V.fromList "abcde") example
-- >>> partTwo (V.fromList "abcde") example
-- "baedc"
-- "abcde"
solve, partTwo :: Vector Char -> [Move] -> Vector Char
solve = foldl' apply
partTwo start moves = states !! idx
  where
    states = iterate (`solve` moves) start
    loop = takeWhile (/= start) (drop 1 states)
    cycleLen = length loop + 1
    idx = 1_000_000_000 `mod` cycleLen

apply :: Vector Char -> Move -> Vector Char
apply v = \case
  Spin n ->
    let k = V.length v - n
     in V.drop k v <> V.take k v
  Exchange i j -> v V.// [(i, v V.! j), (j, v V.! i)]
  Partner a b ->
    let Just i = V.elemIndex a v
        Just j = V.elemIndex b v
     in apply v (Exchange i j)
