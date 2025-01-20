module Day14 (day14, day14TestInput) where

import Algorithm.Search (dijkstra)
import Common
import Data.HashSet qualified as S
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day14TestInput :: String
day14TestInput =
  "U5,R3,D2,L5,U4,R5,D2\n\
  \---\n\
  \U5,R3,D2,L5,U4,R5,D2\n\
  \U6,L1,D2,R3,U2,L1\n\
  \---\n\
  \U20,L1,B1,L2,B1,R2,L1,F1,U1\n\
  \U10,F1,B1,R1,L1,B1,L1,F1,R2,U1\n\
  \U30,L2,F1,R1,B1,R1,F2,U1,F1\n\
  \U25,R1,L2,B1,U1,R2,F1,L2\n\
  \U16,L1,B1,L1,B3,L1,B1,F1\
  \"

day14 :: ECSolution
day14 = parse do
  p1 <- part1 <$> parseSchedule <* P.newline
  P.string "---" <* P.newline
  p2 <- part2 <$> parseSchedule `P.sepEndBy` P.newline
  P.string "---" <* P.newline
  p3 <- part3 . filter (not . null) <$> parseSchedule `P.sepEndBy` P.newline
  pure $ map show [p1, p2, p3]

parseSchedule :: Parser [(Char, Int)]
parseSchedule = p `P.sepBy` P.char ','
  where
    p = (,) <$> P.oneOf "UDRLFB" <*> number'

part1 :: [(Char, Int)] -> Int
part1 = maximum . map (\(_, y, _) -> y) . runSchedule

part2 :: [[(Char, Int)]] -> Int
part2 = S.size . foldr (S.union . S.fromList . runSchedule) S.empty

part3 :: [[(Char, Int)]] -> Int
part3 schedules = minimum scores
  where
    segments :: [[Point3d]]
    segments = map runSchedule schedules
    segments' :: S.HashSet Point3d
    segments' = S.fromList $ concat segments
    leaves :: [Point3d]
    leaves = map last segments
    mainBranch :: [Point3d]
    mainBranch = S.toList $ S.filter (\(x, _, z) -> x == 0 && z == 0) segments'
    murkiness :: Point3d -> Point3d -> Int
    murkiness b l = r
      where
        Just (r, _) = dijkstra ns cost goal l
        ns = filter (`S.member` segments') . point3dNeighbours
        cost _ = const 1
        goal = (== b)
    score :: Point3d -> Int
    score b = sum $ map (murkiness b) leaves
    scores :: [Int]
    scores = map score mainBranch

runSchedule :: [(Char, Int)] -> [Point3d]
runSchedule = flip go (0, 0, 0)
  where
    go [] p = []
    go ((_, 0) : cs) p = go cs p
    go ((c, n) : cs) (x, y, z) = p' : go ((c, pred n) : cs) p'
      where
        p' = case c of
          'L' -> (pred x, y, z)
          'R' -> (succ x, y, z)
          'D' -> (x, pred y, z)
          'U' -> (x, succ y, z)
          'B' -> (x, y, pred z)
          'F' -> (x, y, succ z)
