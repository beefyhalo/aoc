{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Zipper as Z
import Text.Read (readMaybe)

type Reg = Char

type Regs = (Int, Int, Int, Int)

access :: Reg -> Lens' Regs Int
access = \case
  'a' -> _1
  'b' -> _2
  'c' -> _3
  'd' -> _4

data Arg = R Reg | V Int deriving (Show)

data Ins
  = Cpy Arg Arg
  | Inc Reg
  | Dec Reg
  | Jnz Arg Arg
  | Out Arg
  deriving (Show)

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/25.txt"
  print $ solve input

parse :: String -> Z.Zipper Ins
parse = Z.fromNonEmpty . NE.fromList . map parseLine . lines
  where
    parseArg s = maybe (R (head s)) V (readMaybe s)
    parseLine s = case words s of
      ["cpy", x, [y]] -> Cpy (parseArg x) (R y)
      ["inc", [x]] -> Inc x
      ["dec", [x]] -> Dec x
      ["jnz", x, y] -> Jnz (parseArg x) (parseArg y)
      ["out", x] -> Out (parseArg x)

solve :: Z.Zipper Ins -> Int
solve prog = head [a | a <- [0 ..], good (outs prog (a, 0, 0, 0))]
  where
    good xs = take 10 xs == take 10 (cycle [0, 1])
    outs z rs = [o | Just o <- unfoldr (uncurry step) (z, rs)]

step :: Z.Zipper Ins -> Regs -> Maybe (Maybe Int, (Z.Zipper Ins, Regs))
step z regs = (out,) . (,regs') <$> z'
  where
    instr = Z.current z

    val (V i) = i
    val (R r) = regs ^. access r

    out = case instr of
      Out x -> Just (val x)
      _ -> Nothing

    regs' = case instr of
      Cpy src (R dst) -> regs & access dst .~ val src
      Inc r -> regs & access r +~ 1
      Dec r -> regs & access r -~ 1
      _ -> regs

    z' = case instr of
      Jnz x y | val x /= 0 -> moveOffset (val y) z
      _ -> Z.right z

moveOffset :: Int -> Z.Zipper a -> Maybe (Z.Zipper a)
moveOffset n
  | n > 0 = Z.rightN (fromIntegral n)
  | n < 0 = Z.leftN (fromIntegral (abs n))
  | otherwise = Just
