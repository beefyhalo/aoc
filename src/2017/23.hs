{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens
import Control.Monad.Extra (whenJust)
import Control.Monad.State.Strict (State, execState)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V
import Text.Read (readMaybe)

data Prog = Prog
  { _regs :: M.Map Char Int,
    _pc :: Int,
    _mulCount :: Int
  }

makeLenses ''Prog

type Reg = Char

data Arg = R Reg | I Int

data Instr
  = Set Reg Arg
  | Sub Reg Arg
  | Mul Reg Arg
  | Jnz Arg Int

regLens :: Reg -> Lens' Prog Int
regLens r = regs . at r . non 0

evalArg :: Arg -> State Prog Int
evalArg (I n) = pure n
evalArg (R r) = use (regLens r)

main :: IO ()
main = do
  input <- V.fromList . map parse . lines <$> readFile "input/2017/23.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Instr
parse line = case words line of
  ["set", [x], y] -> Set x (parseArg y)
  ["sub", [x], y] -> Sub x (parseArg y)
  ["mul", [x], y] -> Mul x (parseArg y)
  ["jnz", a, off] -> Jnz (parseArg a) (read off)
  where
    parseArg s = maybe (R (head s)) I (readMaybe s)

solve, partTwo :: V.Vector Instr -> Int
solve = _mulCount . flip execState (Prog M.empty 0 0) . runProg
partTwo instrs = sum [1 | b <- [start, start + 17 .. end], not $ isPrime b]
  where
    Set 'b' (I v) = V.head instrs
    start = v * 100 + 100000
    end = start + 17000

    isPrime :: Int -> Bool
    isPrime n = null [x | x <- [2 .. isqrt n], n `mod` x == 0]
      where
        isqrt = floor . sqrt . (fromIntegral :: Int -> Float)

runProg :: V.Vector Instr -> State Prog ()
runProg instrs = do
  i <- use pc
  whenJust (instrs V.!? i) $ \instr ->
    evalInstr instr >> runProg instrs

evalInstr :: Instr -> State Prog ()
evalInstr = \case
  Set x y -> do vy <- evalArg y; regLens x .= vy; pc += 1
  Sub x y -> do vy <- evalArg y; regLens x -= vy; pc += 1
  Mul x y -> do vy <- evalArg y; regLens x *= vy; mulCount += 1; pc += 1
  Jnz a x -> do v <- evalArg a; if v /= 0 then pc += x else pc += 1
