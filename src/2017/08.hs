{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.Map.Strict as MM

type Regs = MM.Map String Int

type Instr = (String, Int, String, String, Int) -- reg, delta, condReg, condOp, condVal

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2017/08.txt"
  print $ solve input

parse :: String -> Instr
parse line = (r, delta, cr, co, read cv)
  where
    [r, op, v, _, cr, co, cv] = words line
    delta = if op == "inc" then read v else -read v

solve :: [Instr] -> (Int, Int)
solve instrs = (maximum final, maximum maxes)
  where
    regss = scanl step MM.empty instrs
    final = last regss
    maxes = [maximum regs | regs <- regss, not $ null regs]

step :: Regs -> Instr -> Regs
step regs (r, delta, cr, co, cv)
  | check condVal cv co = MM.insertWith (+) r delta regs
  | otherwise = regs
  where
    condVal = MM.findWithDefault 0 cr regs

check :: Int -> Int -> String -> Bool
check x y = \case
  ">" -> x > y
  "<" -> x < y
  ">=" -> x >= y
  "<=" -> x <= y
  "==" -> x == y
  "!=" -> x /= y
  _ -> False
