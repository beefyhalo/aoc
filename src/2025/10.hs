{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, many', many1, parseOnly, sepBy, skipSpace)
import Data.Bits (Bits (setBit, shiftL, testBit, xor, (.|.)))
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.Foldable (Foldable (foldl'))
import Data.List (subsequences)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra (Vector, toList, vector)

data Machine = Machine
  { indicator :: [Bool],
    buttons :: [[Int]],
    joltage :: [Int]
  }
  deriving (Show)

buttonMasks :: Machine -> [Int]
buttonMasks (Machine _ bs _) = map (foldl' setBit 0) bs

indicatorMask :: Machine -> Int
indicatorMask (Machine i _ _) = foldr (\b acc -> (acc `shiftL` 1) .|. bool 0 1 b) 0 i

-- $setup
-- >>> import qualified Data.ByteString.Char8 as B
-- >>> input = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
-- >>> example = map parse (B.lines input)

main :: IO ()
main = do
  input <- map Main.parse . B.lines <$> B.readFile "input/2025/10.txt"
  print $ solve input
  print $ partTwo input

parse :: ByteString -> Machine
parse = fromRight (error "failed to parse") . parseOnly machineParser
  where
    machineParser :: Parser Machine
    machineParser = do
      ind <- indicatorParser
      btns <- buttonsParser
      skipSpace
      Machine ind btns <$> joltageParser

    indicatorParser :: Parser [Bool]
    indicatorParser = char '[' *> (map charToBool <$> many' (char '.' <|> char '#') <* char ']')
      where
        charToBool '#' = True
        charToBool _ = False

    buttonParser :: Parser [Int]
    buttonParser = char '(' *> decimal `sepBy` char ',' <* char ')'

    buttonsParser :: Parser [[Int]]
    buttonsParser = many1 (skipSpace *> buttonParser)

    joltageParser :: Parser [Int]
    joltageParser = char '{' *> decimal `sepBy` char ',' <* char '}'

-- >>> solve example
-- 7
solve, partTwo :: [Machine] -> Int
solve = sum . map minPressesXOR
-- >>> partTwo example
-- 33
partTwo = sum . map minPresses

-- >>> map minPressesXOR example
-- [2,3,2]
minPressesXOR :: Machine -> Int
minPressesXOR machine = minimum valid
  where
    target = indicatorMask machine
    masks = buttonMasks machine
    subsets = [combo | combo <- subsequences masks, k <- [1 .. length masks], length combo == k]
    valid = [length combo | combo <- subsets, foldl' xor 0 combo == target]

type Vec = Vector Double

-- >>> map minPresses example
-- [10,12,11]
minPresses :: Machine -> Int
minPresses (Machine _ btns joltages) = fst $ go M.empty (V.fromList $ map fromIntegral joltages)
  where
    nBtns = length btns
    inf = 10 ^ 9 :: Int

    -- Pre-generate all subsets (parity patterns) of button indices
    allSubsets :: [[Int]]
    allSubsets = map (\x -> filter (testBit x) [0 .. nBtns - 1]) [0 .. (2 ^ nBtns - 1) :: Int]

    -- Apply subset to current counters
    applySubset :: [Int] -> Vec -> Vec
    applySubset subset =
      V.imap (\i vi -> vi - fromIntegral (length [j | j <- subset, i `elem` (btns !! j)]))

    -- Recursive memoized function
    go :: M.Map Vec Int -> Vec -> (Int, M.Map Vec Int)
    go memo v
      | V.all (== 0) v = (0, memo)
      | V.any (< 0) v = (inf, memo)
      | Just r <- M.lookup v memo = (r, memo)
      | otherwise =
          let validSubs =
                [ s | s <- allSubsets, let r = applySubset s v, V.all (>= 0) r, V.all (even @Int . floor) r
                ]
              (best, finalMemo) = foldl' process (inf, memo) validSubs
              process (bestSoFar, m) s =
                let half = V.map (/ 2) (applySubset s v)
                    (subCost, m') = go m half
                    cost = 2 * subCost + length s
                 in (min bestSoFar cost, m')
           in (best, M.insert v best finalMemo)
