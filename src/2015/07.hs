{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Char (isDigit)
import qualified Data.Map.Lazy as M

type Wire = String

data Expr
  = Val Int
  | Ref Wire
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | LShift Expr Int
  | RShift Expr Int

-- $setup
-- >>> input = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
-- >>> example = M.fromList $ map parse (lines input)

main :: IO ()
main = do
  input <- M.fromList . map parse . lines <$> readFile "input/2015/07.txt"
  let res = solve input "a"
  print res
  print $ solve (M.insert "b" (Val res) input) "a"

parse :: String -> (Wire, Expr)
parse s = case words s of
  [a, "->", w] -> (w, atom a)
  ["NOT", a, "->", w] -> (w, Not (atom a))
  [a, "AND", b, "->", w] -> (w, And (atom a) (atom b))
  [a, "OR", b, "->", w] -> (w, Or (atom a) (atom b))
  [a, "LSHIFT", n, "->", w] -> (w, LShift (atom a) (read n))
  [a, "RSHIFT", n, "->", w] -> (w, RShift (atom a) (read n))
  where
    atom :: String -> Expr
    atom a
      | all isDigit a = Val (read a)
      | otherwise = Ref a

-- >>> map (solve example) ["d", "e", "f", "g", "h", "i", "x", "y"]
-- [72,507,492,114,65412,65079,123,456]
solve :: M.Map Wire Expr -> Wire -> Int
solve env w = vals M.! w
  where
    vals :: M.Map Wire Int
    vals = M.map evalExpr env

    evalExpr :: Expr -> Int
    evalExpr (Val n) = n
    evalExpr (Ref x) = vals M.! x
    evalExpr (Not a) = complement (evalExpr a) .&. 0xffff
    evalExpr (And a b) = evalExpr a .&. evalExpr b
    evalExpr (Or a b) = evalExpr a .|. evalExpr b
    evalExpr (LShift a n) = evalExpr a `shiftL` n
    evalExpr (RShift a n) = evalExpr a `shiftR` n
