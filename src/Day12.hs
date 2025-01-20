module Day12 (day12, day12TestInput) where

import Common
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day12TestInput :: String
day12TestInput =
  ".............\n\
  \.C...........\n\
  \.B......T....\n\
  \.A......T.T..\n\
  \=============\n\
  \\n\
  \.............\n\
  \.C...........\n\
  \.B......H....\n\
  \.A......T.H..\n\
  \=============\n\
  \\n\
  \6 5\n\
  \6 7\n\
  \10 5\n\
  \\n\
  \"

day12 :: ECSolution
day12 =
  map show . parse do
    p1 <- solve <$> parseGrid <* P.newline
    p2 <- solve <$> parseGrid <* P.newline
    p3 <- part3 <$> parseMeteors <* P.newline
    pure [p1, p2, p3]

parseGrid :: Parser ([(Char, Point2d)], [(Char, Point2d)])
parseGrid = f <$> (P.many1 (P.oneOf ('.' : '=' : csts)) `P.sepEndBy` P.newline)
  where
    (cs, ts) = ("ABC", "TH")
    csts = cs ++ ts
    f lines =
      partition
        ((`elem` ts) . fst)
        [ (v, (i, j))
          | (i, line) <- zip [0 ..] $ reverse lines,
            (j, v) <- zip [0 ..] line,
            v `elem` csts
        ]

parseMeteors :: Parser [Point2d]
parseMeteors = parseMeteor `P.sepEndBy` P.newline
  where
    parseMeteor :: Parser Point2d
    parseMeteor = (,) <$> number <*> number'

solve :: ([(Char, Point2d)], [(Char, Point2d)]) -> Int
solve (ts, cs) = sum $ map (\c -> sum $ mapMaybe (launch c) ts) cs

launch :: (Char, Point2d) -> (Char, Point2d) -> Maybe Int
launch (c, (ci, cj)) (t, (ti, tj)) = if n `mod` 3 /= 0 then Nothing else Just (cv * tv * (n `div` 3))
  where
    di = ci - ti
    dj = tj - cj
    n = dj - di
    cv = fromEnum c - 64
    tv = case t of
      'T' -> 1
      'H' -> 2

part3 :: [Point2d] -> Int
part3 = sum . map f
  where
    f :: Point2d -> Int
    f m = minimum $ mapMaybe (hitMeteor m) [0, 1, 2]

hitMeteor :: Point2d -> Int -> Maybe Int
hitMeteor meteor catapult
  | x < y = Nothing
  | x <= 2 * y = Just $ (catapult + 1) * y
  | r == 0 = Just $ (catapult + 1) * d
  | otherwise = Nothing
  where
    (x, xr) = fst meteor `divMod` 2
    y = snd meteor - x - xr - catapult
    (d, r) = (x + y) `divMod` 3