{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Zipper as Z

data Reg = A | B deriving (Eq, Show)

type Regs = (Int, Int)

getR :: Reg -> Regs -> Int
getR A (a, _) = a
getR B (_, b) = b

setR :: Reg -> Int -> Regs -> Regs
setR A v (_, b) = (v, b)
setR B v (a, _) = (a, v)

data Instr
  = Hlf Reg
  | Tpl Reg
  | Inc Reg
  | Jmp Int
  | Jie Reg Int
  | Jio Reg Int
  deriving (Eq, Show)

-- $setup
-- >>> input = "inc a\njio a, +2\ntpl a\ninc a\n"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2015/23.txt"
  print $ solve input (0, 0)
  print $ solve input (1, 0)

parse :: String -> Z.Zipper Instr
parse = Z.fromNonEmpty . NE.fromList . map parseLine . lines
  where
    parseLine s = case words $ filter (`notElem` ",+") s of
      ["hlf", r] -> Hlf (parseReg r)
      ["tpl", r] -> Tpl (parseReg r)
      ["inc", r] -> Inc (parseReg r)
      ["jmp", o] -> Jmp (read o)
      ["jie", r, o] -> Jie (parseReg r) (read o)
      ["jio", r, o] -> Jio (parseReg r) (read o)
    parseReg "a" = A
    parseReg "b" = B

solve :: Z.Zipper Instr -> Regs -> Regs
solve z r = last $ unfoldr (fmap (\next -> (snd next, next)) . uncurry step) (z, r)

-- solve = curry $ last . unfoldr (fmap (\next -> (snd next, next)) . uncurry step)

-- >>> solve example (0,0)
-- >>> solve example (1,0)
-- (1,0)
-- (6,0)
step :: Z.Zipper Instr -> Regs -> Maybe (Z.Zipper Instr, Regs)
step z regs = (,regs') <$> moveOffset offset z
  where
    instr = Z.current z
    regs' = case instr of
      Hlf r -> setR r (getR r regs `div` 2) regs
      Tpl r -> setR r (getR r regs * 3) regs
      Inc r -> setR r (getR r regs + 1) regs
      _ -> regs
    offset = case instr of
      Jmp o -> o
      Jie r o | even (getR r regs) -> o
      Jio r o | getR r regs == 1 -> o
      _ -> 1

moveOffset :: Int -> Z.Zipper a -> Maybe (Z.Zipper a)
moveOffset n
  | n > 0 = Z.rightN (fromIntegral n)
  | otherwise = Z.leftN (fromIntegral (abs n))
