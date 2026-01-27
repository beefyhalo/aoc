{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Monad (replicateM, replicateM_)
import Control.Monad.State.Strict (State, execState, get, put, state)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, decimal, endOfLine, many', parseOnly, sepBy, skipSpace, string)
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Zipper (Zipper)
import qualified Data.List.NonEmpty.Zipper as Z
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShowId)

type StateName = Char

data Move = L | R deriving (Show)

data Action = Action
  { actWrite :: Int,
    actMove :: Move,
    actNext :: StateName
  }
  deriving (Show)

type Machine = M.Map StateName (Action, Action)

type TM = (StateName, Zipper Int)

moveLeft, moveRight :: Zipper Int -> Zipper Int
moveLeft z = fromJust $ Z.left $ if Z.isStart z then Z.push 0 z else z
moveRight z = fromJust $ Z.right $ if Z.isEnd z then Z.unshift 0 z else z

-- $setup
-- >>> input = "Begin in state A.\nPerform a diagnostic checksum after 6 steps.\n\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\n\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A."
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> B.readFile "input/2017/25.txt"
  print $ solve input

parse :: B.ByteString -> (StateName, Int, Machine)
parse = fromRight (error "failed to parse") . parseOnly machineP
  where
    lexeme s = skipSpace *> string s

    machineP :: Parser (StateName, Int, Machine)
    machineP = do
      start <- string "Begin in state " *> anyChar <* char '.' <* endOfLine
      steps <- string "Perform a diagnostic checksum after " *> decimal <* string " steps."
      states <- stateP `sepBy` skipSpace
      pure (start, steps, M.fromList states)

    stateP :: Parser (StateName, (Action, Action))
    stateP = do
      st <- lexeme "In state " *> anyChar <* char ':'
      a0 <- actionP
      a1 <- actionP
      pure (st, (a0, a1))

    actionP :: Parser Action
    actionP = do
      _ <- lexeme "If the current value is " *> anyChar <* char ':'
      w <- lexeme "- Write the value " *> decimal <* char '.'
      m <- lexeme "- Move one slot to the " *> ((R <$ string "right") <|> (L <$ string "left")) <* char '.'
      n <- lexeme "- Continue with state " *> anyChar <* char '.'
      pure $ Action w m n

-- >>> solve example
-- 3
solve :: (StateName, Int, Machine) -> Int
solve (start, nSteps, machine) = sum final
  where
    (_, final) = execState (replicateM_ nSteps $ step machine) (start, initTape)
    initTape = Z.fromNonEmpty $ NE.singleton 0

step :: Machine -> State TM ()
step m = state $ \(st, tape) ->
  let (act0, act1) = m M.! st
      Action write move next = if Z.current tape == 0 then act0 else act1
      tape' = Z.replace write tape
      tape'' = case move of
        R -> moveRight tape'
        L -> moveLeft tape'
   in ((), (next, tape''))
