{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (evalState, gets, put)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, decimal, many1, parseOnly, string, try)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import GHC.Exts (fromString)

type Input = [Instruction]

data Instruction = Mul Int Int | Do | Dont deriving (Show)

-- (165225049,108830766)
main :: IO ()
main = interact \i -> case parseOnly parser (fromString i) of
  Left err -> show err
  Right input -> show (solve input, partTwo input)

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> example = fromRight undefined $ parseOnly parser "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- >>> example
-- [Mul 2 4,Dont,Mul 5 5,Mul 11 8,Do,Mul 8 5]

parser :: Parser Input
parser = catMaybes <$> many1 junkParser
  where
    junkParser :: Parser (Maybe Instruction)
    junkParser = (Just <$> try instrParser) <|> Nothing <$ anyChar

    instrParser :: Parser Instruction
    instrParser =
      choice
        [ string "mul(" *> liftA2 Mul (decimal <* char ',') decimal <* char ')',
          Do <$ string "do()",
          Dont <$ string "don't()"
        ]

-- >>> solve example
-- 161
solve :: Input -> Int
solve = sum . map go
  where
    go (Mul a b) = a * b
    go _ = 0

-- >>> partTwo example
-- 48
partTwo :: Input -> Int
partTwo = sum . flip evalState True . traverse go
  where
    go (Mul a b) = gets (bool 0 (a * b))
    go Do = 0 <$ put True
    go Dont = 0 <$ put False
