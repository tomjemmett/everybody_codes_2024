module Day09 (day09, day09TestInput) where

import Common
import Control.Parallel.Strategies
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day09TestInput :: String
day09TestInput =
  "2\n\
  \4\n\
  \7\n\
  \16\n\
  \\n\
  \33\n\
  \41\n\
  \55\n\
  \99\n\
  \\n\
  \156488\n\
  \352486\n\
  \546212\n\
  \"

day09 :: ECSolution
day09 =
  map show . parse do
    p1 <- part1 <$> p <* P.newline
    p2 <- part2 <$> p <* P.newline
    p3 <- part3 <$> p
    pure [p1, p2, p3]
  where
    p = number' `P.sepEndBy` P.newline

part1 :: [Int] -> Int
part1 xs = sum $ parMap rseq (dp V.!) xs
  where
    stamps = [1, 3, 5, 10]
    dp = createDp (maximum xs) stamps

part2 :: [Int] -> Int
part2 xs = sum $ parMap rseq (dp V.!) xs
  where
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30]
    dp = createDp (maximum xs) stamps

part3 :: [Int] -> Int
part3 xs = sum $ map go xs
  where
    go b = minimum $ parMap rseq (\i -> dp V.! (b1 - i) + dp V.! (b2 + i)) [0 .. 50]
      where
        b1 = b `div` 2
        b2 = b - b1
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101]
    dp = createDp (maximum xs) stamps

createDp :: Int -> [Int] -> V.Vector Int
createDp maxSparkball stamps = dp
  where
    dp = V.fromList [if x == 0 then 0 else minStamps x | x <- [0 .. maxSparkball]]
    minStamps x = minimum [dp V.! (x - stamp) + 1 | stamp <- stamps, x >= stamp]