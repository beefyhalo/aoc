{-# LANGUAGE LambdaCase #-}

-- $setup
-- >>> example = ["\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""]

main :: IO ()
main = do
  input <- lines <$> readFile "input/2015/08.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve example
-- >>> partTwo example
-- 12
-- 19
solve, partTwo :: [String] -> Int
solve = sum . map (\s -> length s - memLength s)
partTwo = sum . map (\s -> length (encode s) - length s)

memLength :: String -> Int
memLength = \case
  [] -> 0
  ('"' : xs) -> memLength xs -- skip quotes
  ('\\' : 'x' : _ : _ : xs) -> 1 + memLength xs
  ('\\' : _ : xs) -> 1 + memLength xs
  (_ : xs) -> 1 + memLength xs

encode :: String -> String
encode s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c = [c]
