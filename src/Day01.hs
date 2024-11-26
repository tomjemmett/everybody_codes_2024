module Day01 (day01, day01TestInput) where

import Common
import Data.List.Split (chunksOf)

day01TestInput =
  "ABBAC\n\
  \AxBCDDCAxD\n\
  \xBxAAABCDxCC"

day01 :: ECSolution
day01 = map show <$> zipWith solve [1 ..] . lines

solve :: Int -> String -> Int
solve n = sum . map (scores . filter (/= 'x')) . chunksOf n

scores :: String -> Int
scores "" = 0
scores xs = sum s + l * pred l
  where
    s = map score xs
    l = length xs

score :: Char -> Int
score = \case
  'B' -> 1
  'C' -> 3
  'D' -> 5
  _ -> 0