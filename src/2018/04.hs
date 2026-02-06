import qualified Data.IntMap.Monoidal.Strict as M
import Data.List (isInfixOf, sort)
import Data.List.Extra (maximumOn)
import Data.Semigroup (Sum (..))

type Minute = Int

-- $setup
-- >>> input = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . sort . lines <$> readFile "input/2018/04.txt"
  print $ solve input

parse :: [String] -> M.MonoidalIntMap [Minute]
parse input = sleepMap
  where
    (_, _, sleepMap) = foldl' step (0, 0, M.empty) input

    step (g, s, m) line
      | "Guard" `isInfixOf` line = (parseID line, s, m)
      | "falls" `isInfixOf` line = (g, parseMin line, m)
      | "wakes" `isInfixOf` line = (g, 0, M.insertWith (<>) g [s .. parseMin line - 1] m)
      | otherwise = (g, s, m)

    parseID = read . takeWhile (/= ' ') . drop 1 . dropWhile (/= '#')
    parseMin = read . take 2 . drop 15

-- >>> solve example
-- (240,4455)
solve :: M.MonoidalIntMap [Minute] -> (Int, Int)
solve sleepMap = (g1 * m1, g2 * m2)
  where
    stats = M.toList $ fmap (\ms -> (length ms, getPeak ms)) sleepMap
    (g1, (_, (_, m1))) = maximumOn (fst . snd) stats
    (g2, (_, (_, m2))) = maximumOn (fst . snd . snd) stats

getPeak :: [Int] -> (Int, Int)
getPeak ms = M.foldrWithKey' go (0, 0) freqs
  where
    freqs = foldMap (`M.singleton` 1) ms
    go mn (Sum cnt) (maxC, maxM)
      | cnt > maxC = (cnt, mn)
      | otherwise = (maxC, maxM)
