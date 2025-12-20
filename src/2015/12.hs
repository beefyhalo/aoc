{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), decodeFileStrict')
import qualified Data.Aeson.KeyMap as KM
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)

-- $setup
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Aeson.KeyMap as KM
-- >>> example = [Array (V.fromList [Number 1, Number 2, Number 3]), Object (KM.fromList [("a", Number 2), ("b", Number 4)]), Array (V.fromList [Number 1, Object (KM.fromList [("c", "red"), ("b", Number 2)]), Number 3])]

main :: IO ()
main = do
  Just input <- decodeFileStrict' "input/2015/12.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve example
-- [6,6,6]
solve, partTwo :: Value -> Int
solve = fromJust . toBoundedInteger . sum . universeBi
-- >>> map partTwo example
-- [6,6,4]
partTwo = solve . transformBi prune
  where
    prune (Object o) | "red" `elem` KM.elems o = Null
    prune v = v
