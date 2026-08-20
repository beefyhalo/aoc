{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((&&&))
import Control.Comonad (extend, extract)
import Control.Comonad.Store (experiment)
import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, endOfLine, many1, parseOnly, sepBy)
import Data.Bool (bool)
import Data.ByteString qualified as BS (readFile)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (Pair))
import Data.Grid.Sized (Clamped, FocusedGrid (..), Grid, gridFromList, zeroCoord)
import Data.Grid.Sized.Coord (Coord, coordFromTuple, coordRay, offsetCoord)
import Data.Maybe (fromJust)
import GHC.TypeNats (KnownNat, type (<=))

data Cell = X | M | A | S deriving (Eq, Show)

-- The word must not wrap round the edge of the puzzle, so the axes are Clamped:
-- a walk off the end of one stops instead of coming back on the other side.
type Input n = Grid '[Clamped n, Clamped n] Cell

-- (2532,1941)
main :: IO ()
main = either print (print . (solve &&& partTwo)) . parseOnly (parser @140) =<< BS.readFile "input/2024/04.txt"

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> example = fromRight undefined $ parseOnly (parser @10) "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
-- >>> example
-- Grid {unGrid = [M,M,M,S,X,X,M,A,S,M,M,S,A,M,X,M,S,M,S,A,A,M,X,S,X,M,A,A,M,M,M,S,A,M,A,S,M,S,M,X,X,M,A,S,A,M,X,A,M,M,X,X,A,M,M,X,X,A,M,A,S,M,S,M,S,A,S,X,S,S,S,A,X,A,M,A,S,A,A,A,M,A,M,M,M,X,M,M,M,M,M,X,M,X,A,X,M,A,S,X]}

parser :: (KnownNat n) => Parser (Input n)
parser = fromJust . gridFromList <$> many1 cParser `sepBy` endOfLine
  where
    cParser = choice [X <$ char 'X', M <$ char 'M', A <$ char 'A', S <$ char 'S']

data Three a = Three a a a deriving (Eq, Functor, Show, Foldable, Traversable)

type Context a = Compose [] Three a

-- >>> solve example
-- 18
solve :: forall n. (1 <= n, KnownNat n) => Input n -> Int
solve = sum . fmap occurences . extend (\s -> if extract s == X then experiment applyContext s else mempty) . (`FocusedGrid` zeroCoord)
  where
    applyContext :: Coord '[Clamped n, Clamped n] -> Context (Coord '[Clamped n, Clamped n])
    applyContext c =
      Compose
        [ Three i j k
        | dy <- [-1 .. 1],
          dx <- [-1 .. 1],
          (dy, dx) /= (0, 0),
          -- Three steps that way, or nothing: the ray stops at the edge, so a
          -- direction without room for a whole word yields fewer than three
          -- cells and the pattern match drops it.
          [i, j, k] <- [take 3 (coordRay c (coordFromTuple (dy, dx)))]
        ]

    occurences :: Context Cell -> Int
    occurences = length . filter (== Three M A S) . getCompose

data Two a = Two a a deriving (Eq, Show, Functor, Foldable, Traversable)

type ContextTwo a = Compose Maybe (Product Two Two) a

-- >>> partTwo example
-- 9
partTwo :: forall n. (1 <= n, KnownNat n) => Input n -> Int
partTwo = sum . fmap (bool 0 1 . isXmas) . extend (\s -> if extract s == A then experiment applyContext s else Compose Nothing) . (`FocusedGrid` zeroCoord)
  where
    applyContext :: Coord '[Clamped n, Clamped n] -> ContextTwo (Coord '[Clamped n, Clamped n])
    applyContext c =
      -- One diagonal step each way, and all four have to land on the grid.
      Compose $
        traverse (offsetCoord c . coordFromTuple) $
          Pair
            (Two (-1, -1) (1, 1))
            (Two (-1, 1) (1, -1))

    isXmas :: ContextTwo Cell -> Bool
    isXmas (Compose (Just (Pair l r))) = on (&&) (`elem` [Two M S, Two S M]) l r
    isXmas _ = False
