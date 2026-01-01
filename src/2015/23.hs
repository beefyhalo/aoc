{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Lens
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Zipper as Z
import Data.List.Split (wordsBy)

data Reg = A | B deriving (Eq, Show)

type Regs = (Int, Int)

access :: Reg -> Lens' Regs Int
access A = _1
access B = _2

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
    parseLine s = case wordsBy (`elem` " ,+") s of
      ["hlf", r] -> Hlf (parseReg r)
      ["tpl", r] -> Tpl (parseReg r)
      ["inc", r] -> Inc (parseReg r)
      ["jmp", o] -> Jmp (read o)
      ["jie", r, o] -> Jie (parseReg r) (read o)
      ["jio", r, o] -> Jio (parseReg r) (read o)
    parseReg "a" = A
    parseReg "b" = B

-- >>> solve example (0,0)
-- >>> solve example (1,0)
-- (1,0)
-- (6,0)
solve :: Z.Zipper Instr -> Regs -> Regs
solve z r = last $ unfoldr (fmap (\next -> (snd next, next)) . uncurry step) (z, r)

step :: Z.Zipper Instr -> Regs -> Maybe (Z.Zipper Instr, Regs)
step z regs = (,regs') <$> moveOffset offset z
  where
    instr = Z.current z
    regs' = case instr of
      Hlf r -> regs & access r %~ (`div` 2)
      Tpl r -> regs & access r *~ 3
      Inc r -> regs & access r +~ 1
      _ -> regs
    offset = case instr of
      Jmp o -> o
      Jie r o | even (regs ^. access r) -> o
      Jio r o | regs ^. access r == 1 -> o
      _ -> 1

moveOffset :: Int -> Z.Zipper a -> Maybe (Z.Zipper a)
moveOffset n
  | n > 0 = Z.rightN (fromIntegral n)
  | otherwise = Z.leftN (fromIntegral (abs n))
