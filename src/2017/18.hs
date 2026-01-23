{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Text.Read (readMaybe)

data Val = L Int | R Char deriving (Show)

data Inst = Snd Val | Set Char Val | Add Char Val | Mul Char Val | Mod Char Val | Rcv Char | Jgz Val Val

data Machine = Machine
  { _ip :: Int,
    _regs :: Map Char Int,
    _inbox :: [Int],
    _out :: [Int]
  }

makeLenses ''Machine

reg :: Char -> Lens' Machine Int
reg c = regs . at c . non 0

-- $setup
-- >>> input = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2017/18.txt"
  let m = Machine 0 M.empty [] []
  print $ solve input m
  print $ partTwo input (m & reg 'p' .~ 0, m & reg 'p' .~ 1)

parse :: String -> Vector Inst
parse = V.fromList . map parseLine . lines
  where
    parseLine s = case words s of
      ["snd", x] -> Snd (parseVal x)
      ["set", [r], x] -> Set r (parseVal x)
      ["add", [r], x] -> Add r (parseVal x)
      ["mul", [r], x] -> Mul r (parseVal x)
      ["mod", [r], x] -> Mod r (parseVal x)
      ["rcv", [r]] -> Rcv r
      ["jgz", x, y] -> Jgz (parseVal x) (parseVal y)
    parseVal s = maybe (R $ head s) L (readMaybe s)

-- >>> solve example (Machine 0 M.empty [] [])
-- 4
solve :: Vector Inst -> Machine -> Int
solve prog = head . _out . until recovered (fst . step prog)
  where
    recovered m = case prog !? (m ^. ip) of
      Just (Rcv c) -> m ^. reg c /= 0
      _ -> False

-- >>> partTwo example (Machine 0 M.empty [] [], Machine 0 M.empty [] [])
-- 1
partTwo :: Vector Inst -> (Machine, Machine) -> Int
partTwo prog = length . _out . snd . until blocked go
  where
    go (a, b) =
      let (a', sa) = step prog a
          b' = maybe b (\v -> b & inbox <>~ [v]) sa
          (b'', sb) = step prog b'
          a'' = maybe a' (\v -> a' & inbox <>~ [v]) sb
       in (a'', b'')
    blocked (mA, mB) = isBlocked mA && isBlocked mB
    isBlocked m = case prog !? (m ^. ip) of
      Nothing -> True
      Just (Rcv _) -> null (m ^. inbox)
      _ -> False

step :: Vector Inst -> Machine -> (Machine, Maybe Int)
step prog m = case prog !? (m ^. ip) of
  Nothing -> (m, Nothing)
  Just inst -> case inst of
    Set r x -> (m & reg r .~ val x & ip +~ 1, Nothing)
    Add r x -> (m & reg r +~ val x & ip +~ 1, Nothing)
    Mul r x -> (m & reg r *~ val x & ip +~ 1, Nothing)
    Mod r x -> (m & reg r %~ (`mod` val x) & ip +~ 1, Nothing)
    Snd x -> let v = val x in (m & out <>:~ [v] & ip +~ 1, Just v)
    Jgz x y -> (m & ip +~ if val x > 0 then val y else 1, Nothing)
    Rcv r -> case m ^. inbox of
      (v : vs) -> (m & reg r .~ v & inbox .~ vs & ip +~ 1, Nothing)
      [] -> (m, Nothing)
  where
    val (L n) = n
    val (R c) = m ^. reg c
