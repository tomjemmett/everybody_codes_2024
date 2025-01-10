module Day04 (day04, day04TestInput) where

import Common
import Data.List.Split (splitOn)

day04TestInput :: String
day04TestInput =
  "3\n4\n7\n8\n\n\
  \3\n4\n7\n8\n\n\
  \2\n4\n5\n6\n8"

day04 :: ECSolution
day04 input = show <$> [part1 p1, part2 p2, part3 p3]
  where
    [p1, p2, p3] = parseInput input

parseInput :: String -> [[Int]]
parseInput = map (map read . lines) . splitOn "\n\n"

solve :: Int -> [Int] -> Int
solve m = sum . map (abs . subtract m)

part1 :: [Int] -> Int
part1 xs = solve (minimum xs) xs

part2 :: [Int] -> Int
part2 = part1

part3 :: [Int] -> Int
part3 xs = minimum $ map (`solve` xs) xs