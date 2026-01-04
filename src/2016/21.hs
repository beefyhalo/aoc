{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens (ix, (&), (.~))
import Control.Monad (ap)
import Data.Sequence (Seq, deleteAt, elemIndexL, index, insertAt)
import qualified Data.Sequence as Seq

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
  print $ solve "abcdefgh" input
  print $ partTwo "fbgdceah" input

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
-- fromList "decab"
-- fromList "abcde"
solve, partTwo :: Seq Char -> [Instruction] -> Seq Char
solve = foldl' apply
partTwo = foldr (ap apply . flip invert)

apply :: Seq Char -> Instruction -> Seq Char
apply s = \case
  SwapPos i j -> s & ix i .~ index s j & ix j .~ index s i
  SwapLetter a b -> let Just i = elemIndexL a s; Just j = elemIndexL b s in s & ix i .~ b & ix j .~ a
  RotateLeft n -> let k = n `mod` length s in Seq.drop k s <> Seq.take k s
  RotateRight n -> s `apply` RotateLeft (-n)
  RotatePos c -> let Just i = elemIndexL c s in s `apply` RotateRight (1 + i + i `div` 4)
  ReversePos x y -> let (l, rest) = Seq.splitAt x s; (m, r) = Seq.splitAt (y - x + 1) rest in l <> Seq.reverse m <> r
  MovePos i j -> insertAt j (index s i) (deleteAt i s)

invert :: Seq Char -> Instruction -> Instruction
invert s = \case
  RotateLeft n -> RotateRight n
  RotateRight n -> RotateLeft n
  MovePos i j -> MovePos j i
  RotatePos c -> head [t | n <- [0 .. length s - 1], let t = RotateLeft n, apply s t `apply` RotatePos c == s]
  x -> x
