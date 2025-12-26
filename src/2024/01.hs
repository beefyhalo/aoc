{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((***))
import Data.Attoparsec.ByteString.Char8 (Parser, decimal, endOfLine, parseOnly, sepBy, string)
import Data.Foldable (Foldable (foldMap'))
import Data.IntMap.Monoidal.Strict qualified as Map
import Data.List (sort)
import Data.Monoid (Sum (..))
import GHC.Exts (fromString)

type Input = ([Int], [Int])

-- (1603498,25574739)
main :: IO ()
main = interact \i -> case parseOnly parser (fromString i) of
  Left err -> show err
  Right input -> show (solve input, partTwo input)

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> example = fromRight undefined $ parseOnly parser  "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
-- >>> example
-- ([3,4,2,1,3,3],[4,3,5,3,9,3])

parser :: Parser Input
parser = unzip <$> numParser `sepBy` endOfLine
  where
    numParser = liftA2 (,) (decimal <* string "   ") decimal

-- >>> solve example
-- >>> partTwo example
-- 11
-- 31
solve, partTwo :: Input -> Int
solve = sum . uncurry (zipWith distance) . (sort *** sort)
  where
    distance = fmap abs . (-)
partTwo = getSum . Map.foldMapWithKey ((*) . Sum) . uncurry (Map.intersectionWith (*)) . (counts *** counts)
  where
    counts = foldMap' (`Map.singleton` 1)
