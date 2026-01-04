import Data.Bit (Bit (..), invertBits, reverseBits)
import Data.Char (intToDigit)
import qualified Data.Vector.Unboxed as V

-- $setup
-- >>> input = "110010110100"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/16.txt"
  putStrLn $ solve 272 input
  putStrLn $ solve 35651584 input

parse :: String -> V.Vector Bit
parse = V.fromList . map (Bit . (== '1'))

solve :: Int -> V.Vector Bit -> String
solve n bits = intToDigit . fromEnum <$> V.toList result
  where
    filled = V.take n $ until ((>= n) . V.length) dragon bits
    result = until (odd . V.length) checksum filled

-- >>> checksum $ checksum example
-- [1,0,0]
dragon, checksum :: V.Vector Bit -> V.Vector Bit
dragon x = x <> V.singleton (Bit False) <> invertBits (reverseBits x)
checksum v = V.generate (V.length v `div` 2) $ \i ->
  let a = v V.! (2 * i)
      b = v V.! (2 * i + 1)
   in Bit (a == b)
