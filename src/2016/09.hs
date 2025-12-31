{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Functor.Foldable (refold)
import Data.List.Split (splitOn)

data TokenF a = LitF Int a | MarkerF Int Int a a | EndF deriving (Functor)

-- $setup
-- >>> input = "ADVENT\nA(1x5)BC\n(3x3)XYZ\nA(2x2)BCD(2x2)EFG\n(6x1)(1x3)A\nX(8x2)(3x3)ABCY"
-- >>> example = lines input

main :: IO ()
main = do
  input <- readFile "input/2016/09.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> TokenF String
parse = \case
  "" -> EndF
  '(' : s -> MarkerF a b snippet rest'
    where
      (inside, rest) = span (/= ')') s
      [a, b] = read <$> splitOn "x" inside
      (snippet, rest') = splitAt a (drop 1 rest)
  s -> let (plain, rest) = span (/= '(') s in LitF (length plain) rest

-- >>> map solve example
-- >>> map partTwo example
-- [6,7,9,11,6,18]
-- [6,7,9,11,3,20]
solve, partTwo :: String -> Int
solve = refold alg parse
partTwo = refold alg2 parse

alg, alg2 :: TokenF Int -> Int
alg = \case
  EndF -> 0
  LitF n rest -> n + rest
  MarkerF len mul _ rest -> len * mul + rest
alg2 = \case
  EndF -> 0
  LitF n rest -> n + rest
  MarkerF _ mul snippet rest -> mul * snippet + rest
