module Day03 (day03, day03TestInput) where

import Common
import Control.Monad (guard)
import Data.HashSet qualified as H
import Data.List.Split (splitOn)

day03TestInput :: String
day03TestInput =
  "..........\n\
  \..###.##..\n\
  \...####...\n\
  \..######..\n\
  \..######..\n\
  \...####...\n\
  \..........\n\
  \\n\
  \..........\n\
  \..###.##..\n\
  \...####...\n\
  \..######..\n\
  \..######..\n\
  \...####...\n\
  \..........\n\
  \\n\
  \..........\n\
  \..###.##..\n\
  \...####...\n\
  \..######..\n\
  \..######..\n\
  \...####...\n\
  \..........\n\
  \"

day03 :: ECSolution
day03 input = show <$> [part1 i1, part2 i2, part3 i3]
  where
    [i1, i2, i3] = parseInput input

parseInput :: String -> [H.HashSet Point2d]
parseInput = map parsePart . splitOn "\n\n"

parsePart :: String -> H.HashSet Point2d
parsePart input =
  H.fromList
    [ (i, j)
      | (i, xs) <- zip [0 ..] $ lines input,
        (j, x) <- zip [0 ..] xs,
        x == '#'
    ]

part1 :: H.HashSet Point2d -> Int
part1 = solve point2dNeighbours 4

part2 :: H.HashSet Point2d -> Int
part2 = solve point2dNeighbours 4

part3 :: H.HashSet Point2d -> Int
part3 = solve point2dNeighboursDiags 8

solve :: (Point2d -> [Point2d]) -> Int -> H.HashSet Point2d -> Int
solve _ _ grid | H.null grid = 0
solve fn n grid = H.size grid + solve fn n grid'
  where
    grid' = mine fn n grid

mine :: (Point2d -> [Point2d]) -> Int -> H.HashSet Point2d -> H.HashSet Point2d
mine fn n grid = H.fromList do
  p <- H.toList grid
  let ns = countTrue id [H.member n grid | n <- fn p]
  guard $ ns == n
  pure p