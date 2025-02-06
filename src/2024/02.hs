{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.ByteString.Char8 (Parser, decimal, endOfLine, parseOnly, sepBy, space)
import Data.List (inits, tails)
import GHC.Exts (fromString)

type Input = [Report]

type Report = [Integer]

-- (502,544)
main :: IO ()
main = interact \i -> case parseOnly parser (fromString i) of
  Left err -> show err
  Right input -> show (solve input, partTwo input)

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> example = fromRight undefined $ parseOnly parser "7 6 4 2 1\r\n1 2 7 8 9\r\n9 7 6 2 1\r\n1 3 2 4 5\r\n8 6 4 4 1\r\n1 3 6 7 9"
-- >>> example
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]

parser :: Parser Input
parser = decimal `sepBy` space `sepBy` endOfLine

-- >>> safe [7, 6, 4, 2, 1]
-- >>> safe [1, 2, 7, 8, 9]
-- True
-- False
safe :: Report -> Bool
safe r = cond1 && cond2
  where
    neighbors = zipWith (-) r (tail r)
    cond1 = all (> 0) neighbors || all (< 0) neighbors
    cond2 = all (flip elem [1 .. 3] . abs) neighbors

-- >>> solve example
-- 2
solve :: Input -> Int
solve = length . filter safe

-- >>> partTwo example
-- 4
partTwo :: Input -> Int
partTwo = length . filter (any safe . alter)
  where
    alter r = r : zipWith (++) (inits r) (tail $ tails r)
