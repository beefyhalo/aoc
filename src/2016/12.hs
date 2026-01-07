{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Zipper as Z
import Text.Read (readMaybe)

type Reg = Char

type Regs = (Int, Int, Int, Int)

access :: Reg -> Lens' Regs Int
access 'a' = _1
access 'b' = _2
access 'c' = _3
access 'd' = _4

data Arg = R Reg | V Int deriving (Eq, Show)

data Instr
  = Cpy Arg Reg
  | Inc Reg
  | Dec Reg
  | Jnz Arg Arg
  deriving (Eq, Show)

-- $setup
-- >>> input = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/12.txt"
  print $ solve input (0, 0, 0, 0) ^. _1
  print $ solve input (0, 0, 1, 0) ^. _1

parse :: String -> Z.Zipper Instr
parse = Z.fromNonEmpty . NE.fromList . map parseLine . lines
  where
    parseLine s = case words s of
      ["cpy", x, [y]] -> Cpy (parseVal x) y
      ["inc", [x]] -> Inc x
      ["dec", [x]] -> Dec x
      ["jnz", x, y] -> Jnz (parseVal x) (parseVal y)
    parseVal s = maybe (R (head s)) V (readMaybe s)

-- >>> solve example (0,0,0,0)
-- (42,0,0,0)
solve :: Z.Zipper Instr -> Regs -> Regs
solve z r = maybe r (uncurry solve) (step z r)

step :: Z.Zipper Instr -> Regs -> Maybe (Z.Zipper Instr, Regs)
step z regs = (,regs') <$> moveOffset offset z
  where
    instr = Z.current z

    val (V i) = i
    val (R r) = regs ^. access r

    regs' = case instr of
      Cpy v r -> regs & access r .~ val v
      Inc r -> regs & access r +~ 1
      Dec r -> regs & access r -~ 1
      _ -> regs

    offset = case instr of
      Jnz x y | val x /= 0 -> val y
      _ -> 1

moveOffset :: Int -> Z.Zipper a -> Maybe (Z.Zipper a)
moveOffset n
  | n > 0 = Z.rightN (fromIntegral n)
  | n < 0 = Z.leftN (fromIntegral (abs n))
  | otherwise = Just
