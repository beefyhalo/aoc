{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, many', many1, parseOnly, sepBy, skipSpace)
import Data.Bits (Bits (setBit, shiftL, xor, (.|.)))
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.Foldable (Foldable (foldMap', foldl'))
import Data.List (subsequences, unfoldr)
import qualified Data.List as L
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Linear.Vector as V
import Linear hiding (vector)
import Numeric.LinearAlgebra

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
-- 31
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

-- >>> map minPresses example
-- [10,11,10]
minPresses :: Machine -> Int
minPresses (Machine _ btns target) = search x0 ns
  where
    a = buildMatrix btns
    b = vector $ map fromIntegral target
    x0 = a <\> b
    ns = nullspaceBasis a

type Vec = Vector Double

type Mat = Matrix Double

buildMatrix :: [[Int]] -> Mat
buildMatrix btns =
  fromColumns [vector [bool 0 1 (i `elem` btn) | i <- [0 .. n]] | btn <- btns]
  where
    n = maximum (concat btns)

nullspaceBasis :: Mat -> [Vec]
nullspaceBasis a = drop (rank a) (toColumns (tr vt))
  where
    (_, _, vt) = svd a

search :: Vec -> [Vec] -> Int
search x0 basis = go (ceiling (sumElements x0)) x0
  where
    go best cur
      | minElement cur < 0 = best
      | sumElements cur >= fromIntegral best = best
      | otherwise =
          foldl'
            (\b v -> min (go b (cur  + v)) (go b (cur - v)))
            (floor (sumElements cur))
            basis
