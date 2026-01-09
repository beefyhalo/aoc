{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, endOfInput, parseOnly, sepBy)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as B

type Result = (Int, Int)

main :: IO ()
main = do
  input <- B.readFile "input/2017/09.txt"
  print $ parseOnly stream input

-- >>> map (parseOnly stream) ["{{{},{},{{}}}}", "{{<ab>},{<ab>},{<ab>},{<ab>}}"]
-- [Right (16,0),Right (9,8)]
stream :: Parser Result
stream = group 1 <* endOfInput

group :: Int -> Parser Result
group depth = do
  char '{'
  rs <- sepBy (element (depth + 1)) (char ',')
  char '}'
  let (scores, garbage) = unzip rs
  pure (depth + sum scores, sum garbage)

element :: Int -> Parser Result
element depth = group depth <|> garbage

garbage :: Parser Result
garbage = do
  char '<'
  n <- garbageChars 0
  char '>'
  pure (0, n)

garbageChars :: Int -> Parser Int
garbageChars !n =
  (char '!' *> anyChar *> garbageChars n)
    <|> (lookAhead (char '>') *> pure n)
    <|> (anyChar *> garbageChars (n + 1))
