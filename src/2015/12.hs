{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (..), decodeFileStrict')
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)

-- $setup
-- >>> import Data.Aeson.KeyMap qualified as KM
-- >>> import Data.Vector qualified as V
-- >>> examples = [Array (V.fromList [Number 1, Number 2, Number 3]), Object (KM.fromList [("a", Number 2), ("b", Number 4)]), Array (V.fromList [Number 1, Object (KM.fromList [("c", "red"), ("b", Number 2)]), Number 3])]

main :: IO ()
main = do
  Just input <- decodeFileStrict' "input/2015/12.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve examples
-- >>> map partTwo examples
-- [6,6,6]
-- [6,6,4]
solve, partTwo :: Value -> Int
solve = fromJust . toBoundedInteger . sum . universeBi
partTwo = solve . transformBi prune
  where
    prune (Object o) | "red" `elem` o = Null
    prune v = v
