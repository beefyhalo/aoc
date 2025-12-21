{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List (findIndex)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

type Aunt = M.Map String Int

mfcsam :: Aunt
mfcsam =
  M.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2015/16.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Aunt
parse s = M.fromList [(k, read v) | [k, v] <- splitOn ": " <$> splitOn ", " props]
  where
    props = drop 2 $ dropWhile (/= ':') s

solve, partTwo :: [Aunt] -> Int
solve = (+ 1) . fromJust . findIndex (`M.isSubmapOf` mfcsam)
partTwo = (+ 1) . fromJust . findIndex (match mfcsam)

match :: Aunt -> Aunt -> Bool
match sue = and . M.intersectionWithKey check sue
  where
    check = \case
      "cats" -> (<)
      "trees" -> (<)
      "pomeranians" -> (>)
      "goldfish" -> (>)
      _ -> (==)