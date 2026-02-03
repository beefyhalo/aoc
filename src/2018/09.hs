{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Lens
import Control.Monad.State.Strict (State, execState)
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S
import Text.Read (readMaybe)

data Game = Game
  { _circle :: S.Seq Int,
    _scores :: M.IntMap Int
  }

makeLenses ''Game

rotate :: Int -> State Game ()
rotate n = do
  c <- use circle
  let len = S.length c
      (l, r) = S.splitAt (n `mod` len) c
  circle .= r <> l

main :: IO ()
main = do
  [n, m] <- mapMaybe readMaybe . words <$> readFile "input/2018/09.txt"
  print $ solve n m
  print $ solve n (m * 100)

-- >>> solve 10 1618
-- >>> solve 13 7999
-- 8317
-- 146373
solve :: Int -> Int -> Int
solve numPlayers maxMarble = maximum $ final ^. scores
  where
    initial = Game (S.singleton 0) M.empty
    final = execState (traverse_ (play numPlayers) [1 .. maxMarble]) initial

play :: Int -> Int -> State Game ()
play numPlayers m
  | m `rem` 23 == 0 = do
      rotate (-7)
      c <- use circle
      let !(removed S.:<| rest) = c
      circle .= rest
      scores . at (m `rem` numPlayers) . non 0 += m + removed
  | otherwise = do
      rotate 2
      circle <|= m
