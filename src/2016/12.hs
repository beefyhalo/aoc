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
access 'a' = _1
access 'b' = _2
access 'c' = _3
access 'd' = _4

data Val = VInt Int | VReg Reg deriving (Eq, Show)

data Instr
  = Cpy Val Reg
  | Inc Reg
  | Dec Reg
  | Jnz Val Val
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
    parseVal s = maybe (VReg (head s)) VInt (readMaybe s)

-- >>> solve example (0,0,0,0)
-- (42,0,0,0)
solve :: Z.Zipper Instr -> Regs -> Regs
solve z r = last $ unfoldr (fmap (\next -> (snd next, next)) . uncurry step) (z, r)

step :: Z.Zipper Instr -> Regs -> Maybe (Z.Zipper Instr, Regs)
step z regs = (,regs') <$> moveOffset offset z
  where
    instr = Z.current z

    val (VInt i) = i
    val (VReg r) = regs ^. access r

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
