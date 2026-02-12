{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative ((<|>))
import Control.Lens (at, non, uses, (%=))
import Control.Monad (foldM)
import Control.Monad.State.Strict (State, execState)
import Data.Attoparsec.ByteString.Char8 (char, inClass, many', parseOnly, satisfy, sepBy1)
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

data Regex = Seq [Regex] | Alt [Regex] | Dir Char

type Pos = (Int, Int)

step :: Pos -> Char -> Pos
step (x, y) = \case 'N' -> (x, y - 1); 'S' -> (x, y + 1); 'E' -> (x + 1, y); 'W' -> (x - 1, y)

-- $setup
-- >>> input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> B.readFile "input/2018/20.txt"
  print $ solve input

parse :: B.ByteString -> Regex
parse = either error id . parseOnly parser
  where
    parser = char '^' *> parseRegex <* char '$'
    parseRegex = Seq <$> many' termP
    termP = (Dir <$> satisfy (inClass "NSEW")) <|> branchP
    branchP = char '(' *> (Alt <$> (parseRegex `sepBy1` char '|')) <* char ')'

-- >>> solve example
-- (23,0)
solve :: Regex -> (Int, Int)
solve input = (maximum dists, length $ M.filter (>= 1000) dists)
  where
    dists = execState (eval (S.singleton (0, 0)) input) (M.singleton (0, 0) 0)

eval :: Set Pos -> Regex -> State (Map Pos Int) (Set Pos)
eval frontier = \case
  Seq rs -> foldM eval frontier rs
  Alt rs -> S.unions <$> traverse (eval frontier) rs
  Dir d -> foldM advance S.empty frontier
    where
      advance acc p = do
        new <- uses (at p . non 0) (+ 1)
        let q = step p d
        at q %= Just . maybe new (min new)
        pure $ S.insert q acc
