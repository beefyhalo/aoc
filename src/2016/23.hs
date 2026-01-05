{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Applicative ((<|>))
import Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Zipper as Z
import Data.Maybe (fromMaybe)
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
  | Tgl Arg
  deriving (Show)

-- $setup
-- >>> input = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/23.txt"
  print $ solve input (7, 0, 0, 0) ^. _1
  print $ solve input (12, 0, 0, 0) ^. _1

parse :: String -> Z.Zipper Ins
parse = Z.fromNonEmpty . NE.fromList . map parseLine . lines
  where
    parseArg s = maybe (R (head s)) V (readMaybe s)
    parseLine s = case words s of
      ["cpy", x, [y]] -> Cpy (parseArg x) (R y)
      ["inc", [x]] -> Inc x
      ["dec", [x]] -> Dec x
      ["jnz", x, y] -> Jnz (parseArg x) (parseArg y)
      ["tgl", x] -> Tgl (parseArg x)

-- >>> solve example (7, 0, 0, 0)
-- (3,0,0,0)
solve :: Z.Zipper Ins -> Regs -> Regs
solve z rs = maybe rs (uncurry solve) $ step z rs

get :: Regs -> Arg -> Int
get rs = \case
  V n -> n
  R r -> rs ^. access r

toggle :: Ins -> Ins
toggle = \case
  Inc r -> Dec r
  Dec r -> Inc r
  Tgl (R r) -> Inc r
  Jnz x y -> Cpy x y
  Cpy x y -> Jnz x y

ixRel :: Int -> Traversal' (Z.Zipper a) a
ixRel n f z = case moveOffset n z of
  Nothing -> pure z
  Just focused ->
    let back = moveOffset (-n)
     in fromMaybe z . back . (`Z.replace` focused) <$> f (Z.current focused)

step :: Z.Zipper Ins -> Regs -> Maybe (Z.Zipper Ins, Regs)
step z regs = optimize z regs <|> fmap (,regs') z'
  where
    instr = Z.current z
    regs' = case instr of
      Cpy src (R dst) -> regs & access dst .~ get regs src
      Inc r -> regs & access r +~ 1
      Dec r -> regs & access r -~ 1
      _ -> regs
    z' = case instr of
      Tgl arg -> Z.right (z & ixRel (get regs arg) %~ toggle)
      Jnz x y -> moveOffset (if get regs x /= 0 then get regs y else 1) z
      _ -> Z.right z

-- detect a += b * d pattern
-- Pattern: inc a, dec c, jnz c -2, dec d, jnz d -5
optimize :: Z.Zipper Ins -> Regs -> Maybe (Z.Zipper Ins, Regs)
optimize z rs = case Z.current z : take 5 (Z.rights z) of
  [Cpy (R b) (R c), Inc a, Dec c', Jnz (R c'') (V (-2)), Dec d, Jnz (R d') (V (-5))]
    | c == c' && c == c'' && d == d' ->
        let rs' =
              rs
                & access a +~ (rs ^. access b) * (rs ^. access d)
                & access c .~ 0
                & access d .~ 0
         in (,rs') <$> Z.rightN 6 z
  _ -> Nothing

moveOffset :: Int -> Z.Zipper a -> Maybe (Z.Zipper a)
moveOffset n
  | n > 0 = Z.rightN (fromIntegral n)
  | n < 0 = Z.leftN (fromIntegral (abs n))
  | otherwise = Just
