{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Control.Applicative (Alternative (many), (<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, parseOnly, skipSpace)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (fold)
import Data.List (foldl', transpose)

-- $setup
-- >>> input = "123 328  51 64\n45 64  387 23\n6 98  215 314\n*   +   *   +"
-- >>> example = lines input

main :: IO ()
main = do
  input <- lines <$> readFile "input/2025/06.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve example
-- >>> partTwo example
-- 4277556
-- 847058
solve, partTwo :: [String] -> Int
solve = sum . map (compute . reverse) . transpose . map words
  where
    compute = \case
      ("+" : nums) -> sum $ map read nums
      ("*" : nums) -> product $ map read nums

type Col = Either Int Char

partTwo = fst . foldl' go (0, []) . concatMap parseCol . reverse . transpose
  where
    go (!total, buffer) = \case
      Left !num -> (total, num : buffer)
      Right '+' -> (total + sum buffer, [])
      Right '*' -> (total + product buffer, [])

    parseCol :: String -> [Col]
    parseCol = fold . parseOnly columnParser . BC.pack

    columnParser :: Parser [Col]
    columnParser = liftA2 (<>)
      (fmap Left <$> (skipSpace *> many decimal))
      (fmap Right <$> (skipSpace *> many operation))

    operation = char '+' <|> char '*'
