module Days
  ( day01,
    day01TestInput,
    day02,
    day02TestInput,
    day03,
    day03TestInput,
    day04,
    day04TestInput,
    day05,
    day05TestInput,
    day06,
    day06TestInput,
    day07,
    day07TestInput,
    day08,
    day08TestInput,
    day09,
    day09TestInput,
    day10,
    day10TestInput,
    day11,
    day11TestInput,
    day12,
    day12TestInput,
    day13,
    day13TestInput,
    day14,
    day14TestInput,
    day15,
    day15TestInput,
    day16,
    day16TestInput,
    day17,
    day17TestInput,
    day18,
    day18TestInput,
    day19,
    day19TestInput,
    day20,
    day20TestInput,
    runDay,
  )
where

import Control.Monad (when)
import Day01 (day01, day01TestInput)
import Day02 (day02, day02TestInput)
import Day03 (day03, day03TestInput)
import Day04 (day04, day04TestInput)
import Day05 (day05, day05TestInput)
import Day06 (day06, day06TestInput)
import Day07 (day07, day07TestInput)
import Day08 (day08, day08TestInput)
import Day09 (day09, day09TestInput)
import Day10 (day10, day10TestInput)
import Day11 (day11, day11TestInput)
import Day12 (day12, day12TestInput)
import Day13 (day13, day13TestInput)
import Day14 (day14, day14TestInput)
import Day15 (day15, day15TestInput)
import Day16 (day16, day16TestInput)
import Day17 (day17, day17TestInput)
import Day18 (day18, day18TestInput)
import Day19 (day19, day19TestInput)
import Day20 (day20, day20TestInput)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)

days =
  [ (day01, "inputs/day01.txt"),
    (day02, "inputs/day02.txt"),
    (day03, "inputs/day03.txt"),
    (day04, "inputs/day04.txt"),
    (day05, "inputs/day05.txt"),
    (day06, "inputs/day06.txt"),
    (day07, "inputs/day07.txt"),
    (day08, "inputs/day08.txt"),
    (day09, "inputs/day09.txt"),
    (day10, "inputs/day10.txt"),
    (day11, "inputs/day11.txt"),
    (day12, "inputs/day12.txt"),
    (day13, "inputs/day13.txt"),
    (day14, "inputs/day14.txt"),
    (day15, "inputs/day15.txt"),
    (day16, "inputs/day16.txt"),
    (day17, "inputs/day17.txt"),
    (day18, "inputs/day18.txt"),
    (day19, "inputs/day19.txt"),
    (day20, "inputs/day20.txt")
  ]

runDay :: Int -> IO ()
runDay day = do
  let (fn, file) = days !! pred day
  fileExists <- doesFileExist file
  when fileExists do
    input <- readFile file

    when (input /= "") $ timeIt do
      putStrLn $ replicate 80 '-'
      putStr $ "Day: " ++ show day
      putStrLn ""
      putStr $ unlines $ fn input