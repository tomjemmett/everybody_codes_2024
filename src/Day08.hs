module Day08 (day08, day08TestInput) where

import Common
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day08TestInput :: String
day08TestInput =
  "13\n\
  \3 5 50\n\
  \2 5 160"

day08 :: ECSolution
day08 =
  map show . parse do
    p1 <- part1 <$> number
    p2 <- part2 <$> number <*> number <*> number
    p3 <- part3 <$> number <*> number <*> number
    pure [p1, p2, p3]

part1 :: Int -> Int
part1 x = (v ^ 2 - x) * (2 * v - 1)
  where
    v = ceiling $ sqrt $ fromIntegral x

part2 :: Int -> Int -> Int -> Int
part2 p a b = go [1] 3 1
  where
    go hs n l =
      if b > r
        then go hs' (n + 2) t
        else (r - b) * n
      where
        t = p * l `mod` a
        hs' = t : t : map (+ t) hs
        r = sum hs'

part3 :: Int -> Int -> Int -> Int
part3 p a b = go [1] 3 1
  where
    go hs n l =
      if b > r
        then go hs' (n + 2) t
        else r - b
      where
        t = (p * l `mod` a) + a
        hs' = t : t : map (+ t) hs
        r = 2 * t + sum (map (\h -> h - p * n * h `mod` a) $ drop 2 hs')
