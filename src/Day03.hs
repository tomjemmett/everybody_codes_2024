module Day03 (day03, day03TestInput) where

import Common
import Control.Monad (guard)
import Data.HashSet qualified as H
import Data.List.Split (splitOn)

type Grid = H.HashSet Point2d

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
day03 = solveDay [part1, part2, part3] parseInput

parseInput :: String -> [Grid]
parseInput = map parsePart . splitOn "\n\n"

parsePart :: String -> Grid
parsePart input =
  H.fromList
    [ (i, j)
      | (i, xs) <- zip [0 ..] $ lines input,
        (j, x) <- zip [0 ..] xs,
        x == '#'
    ]

part1, part2, part3 :: Grid -> Int
part1 = solve point2dNeighbours
part2 = part1
part3 = solve point2dNeighboursDiags

solve :: (Point2d -> [Point2d]) -> Grid -> Int
solve _ grid | H.null grid = 0
solve fn grid = H.size grid + solve fn grid'
  where
    grid' = H.fromList do
      p <- H.toList grid
      guard $ all (`H.member` grid) $ fn p
      pure p