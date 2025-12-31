import Data.List (maximumBy, minimumBy, transpose)
import qualified Data.Map.Monoidal.Strict as M
import Data.Monoid
import Data.Ord (comparing)

-- $setup
-- >>> input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/06.txt"
  putStrLn $ solve input
  putStrLn $ partTwo input

parse :: String -> [M.MonoidalMap Char (Sum Int)]
parse = map freq . transpose . lines
  where
    freq = foldMap (`M.singleton` 1)

-- >>> solve example
-- >>> partTwo example
-- "easter"
-- "advent"
solve, partTwo :: [M.MonoidalMap Char (Sum Int)] -> String
solve = map $ fst . maximumBy (comparing snd) . M.toList
partTwo = map $ fst . minimumBy (comparing snd) . M.toList
