{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2015/07.txt"
  let res = solve input "a"
  print res
  print $ partTwo res input "a"

parse :: String -> M.Map Wire Expr
parse = M.fromList . map parseLine . lines
  where
    parseLine s = case words s of
      [a, "->", w] -> (w, atom a)
      ["NOT", a, "->", w] -> (w, Not (atom a))
      [a, "AND", b, "->", w] -> (w, And (atom a) (atom b))
      [a, "OR", b, "->", w] -> (w, Or (atom a) (atom b))
      [a, "LSHIFT", n, "->", w] -> (w, LShift (atom a) (read n))
      [a, "RSHIFT", n, "->", w] -> (w, RShift (atom a) (read n))

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
    evalExpr = \case
      Val n -> n
      Ref x -> vals M.! x
      Not a -> complement (evalExpr a) .&. 0xffff
      And a b -> evalExpr a .&. evalExpr b
      Or a b -> evalExpr a .|. evalExpr b
      LShift a n -> evalExpr a `shiftL` n
      RShift a n -> evalExpr a `shiftR` n

partTwo :: Int -> M.Map Wire Expr -> Wire -> Int
partTwo res = solve . M.insert "b" (Val res)