{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, many', many1, parseOnly, sepBy, skipSpace)
import Data.Bits (Bits (..))
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.Foldable (Foldable (foldl'))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (subsequences)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra (Vector)

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
-- >>> input = B.pack "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
-- >>> example = map parse (B.lines input)

main :: IO ()
main = do
  input <- map parse . B.lines <$> B.readFile "input/2025/10.txt"
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

type Vec = Vector Int

-- >>> map minPresses example
-- [10,12,11]
minPresses :: Machine -> Int
minPresses (Machine _ btns joltages) = fst $ go M.empty (V.fromList joltages)
  where
    n = 1 + maximum (concat btns)

    -- For each counter, a bitmask of which buttons affect it
    counterMasks :: Vec
    counterMasks = V.generate n $ \i -> foldl' setBit 0 [b | (b, bs) <- zip [0 ..] btns, i `elem` bs]

    -- Pre-group subsets by parity signature
    parityGroups :: IntMap [Int]
    parityGroups = IM.fromListWith (++) [(sig s, [s]) | s <- [0 .. 1 `shiftL` length btns - 1]]
      where
        sig s = foldl' (\acc i -> parityKey acc i (popCount (s .&. counterMasks V.! i))) 0 [0 .. n - 1]

    parityKey acc i vi = if odd vi then setBit acc i else acc

    -- Apply a subset to a vector
    apply :: Int -> Vec -> Vec
    apply s = V.imap $ \i -> subtract (popCount (s .&. counterMasks V.! i))

    -- Find all subsets where remaining voltages are even, divide by 2 and recurse
    go :: M.Map Vec Int -> Vec -> (Int, M.Map Vec Int)
    go memo v
      | V.all (== 0) v = (0, memo)
      | Just r <- M.lookup v memo = (r, memo)
      | otherwise = (best, M.insert v best finalMemo)
      where
        key = V.ifoldl' parityKey 0 v
        subs = IM.findWithDefault [] key parityGroups
        (best, finalMemo) = foldl' step (10000000, memo) subs

        step (!bestSoFar, m) s
          | V.any (< 0) r = (bestSoFar, m)
          | otherwise = (min bestSoFar cost, m')
          where
            r = apply s v
            half = V.map (`div` 2) r
            (subCost, m') = go m half
            cost = 2 * subCost + popCount s
