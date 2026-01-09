{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Char8 as B

data State = Normal | Garbage | Canceled deriving (Show)

data Context = Context
  { depth, score, garbage :: !Int,
    mode :: State
  }
  deriving (Show)

main :: IO ()
main = do
  input <- B.readFile "input/2017/09.txt"
  print $ solve input

-- >>> map solve ["{{{},{},{{}}}}", "{{<ab>},{<ab>},{<ab>},{<ab>}}"]
-- [(16,0),(9,8)]
solve :: B.ByteString -> (Int, Int)
solve input = (score, garbage)
  where
    Context {..} = B.foldl' step (Context 0 0 0 Normal) input

step :: Context -> Char -> Context
step ctx@Context {..} c = case mode of
  Canceled -> ctx {mode = Garbage}
  Garbage -> case c of
    '!' -> ctx {mode = Canceled}
    '>' -> ctx {mode = Normal}
    _ -> ctx {garbage = garbage + 1}
  Normal -> case c of
    '<' -> ctx {mode = Garbage}
    '{' -> ctx {depth = depth + 1}
    '}' -> ctx {score = score + depth, depth = depth - 1}
    _ -> ctx
