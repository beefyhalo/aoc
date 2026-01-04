{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Monad (ap)
import Data.List (elemIndex)

data Instruction
  = SwapPos Int Int
  | SwapLetter Char Char
  | RotateLeft Int
  | RotateRight Int
  | RotatePos Char
  | ReversePos Int Int
  | MovePos Int Int
  deriving (Show, Eq)

-- $setup
-- >>> input = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/21.txt"
  putStrLn $ solve "abcdefgh" input
  putStrLn $ partTwo "fbgdceah" input

parse :: String -> Instruction
parse line = case words line of
  ["swap", "position", x, "with", "position", y] -> SwapPos (read x) (read y)
  ["swap", "letter", [x], "with", "letter", [y]] -> SwapLetter x y
  ["rotate", "left", x, _] -> RotateLeft (read x)
  ["rotate", "right", x, _] -> RotateRight (read x)
  ["rotate", "based", "on", "position", "of", "letter", [x]] -> RotatePos x
  ["reverse", "positions", x, "through", y] -> ReversePos (read x) (read y)
  ["move", "position", x, "to", "position", y] -> MovePos (read x) (read y)
  _ -> error $ "Parse error: " ++ line

-- >>> solve "abcde" example
-- >>> partTwo "decab" example
-- "decab"
-- "abcde"
solve, partTwo :: String -> [Instruction] -> String
solve = foldl' apply
partTwo = foldr (ap apply . flip invert)

apply :: String -> Instruction -> String
apply s = \case
  SwapPos x y -> [if i == x then s !! y else if i == y then s !! x else c | (i, c) <- zip [0 ..] s]
  SwapLetter a b -> [if c == a then b else if c == b then a else c | c <- s]
  RotateLeft n -> let k = n `mod` length s in drop k s ++ take k s
  RotateRight n -> apply s (RotateLeft (-n))
  RotatePos c -> let Just i = elemIndex c s in apply s (RotateRight $ 1 + i + i `div` 4)
  ReversePos x y -> take x s ++ reverse (take (y - x + 1) (drop x s)) ++ drop (y + 1) s
  MovePos x y -> let s' = take x s ++ drop (x + 1) s in take y s' ++ [s !! x] ++ drop y s'

invert :: String -> Instruction -> Instruction
invert s = \case
  RotateLeft n -> RotateRight n
  RotateRight n -> RotateLeft n
  MovePos i j -> MovePos j i
  RotatePos c -> head [t | n <- [0 .. length s - 1], let t = RotateLeft n, apply s t `apply` RotatePos c == s]
  x -> x
