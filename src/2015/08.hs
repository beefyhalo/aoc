{-# LANGUAGE LambdaCase #-}

module Main (main) where

-- $setup
-- >>> example = ["\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""]

main :: IO ()
main = do
  input <- lines <$> readFile "input/2015/08.txt"
  print $ solve input

solve :: [String] -> Int
solve input = codeLen - memLen
  where
    codeLen = sum (map length input)
    memLen = sum (map memLength input)

-- >>> map memLength example
-- [0,3,7,1]
memLength :: String -> Int
memLength = \case
  [] -> 0
  ('"' : xs) -> memLength xs -- skip quotes
  ('\\' : 'x' : _ : _ : xs) -> 1 + memLength xs
  ('\\' : _ : xs) -> 1 + memLength xs
  (_ : xs) -> 1 + memLength xs
