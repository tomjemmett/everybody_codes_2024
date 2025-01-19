module Day13 (day13, day13TestInput) where

import Algorithm.Search (aStar)
import Common
import Data.Bifunctor (second)
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import Data.List (find, intercalate)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day13TestInput :: String
day13TestInput = intercalate "\n" [day13TestInputPart1, day13TestInputPart1, day13TestInputPart3]

day13TestInputPart1 =
  "#######\n\
  \#6769##\n\
  \S50505E\n\
  \#97434#\n\
  \#######\n\
  \"

day13TestInputPart3 =
  "SSSSSSSSSSS\n\
  \S674345621S\n\
  \S###6#4#18S\n\
  \S53#6#4532S\n\
  \S5450E0485S\n\
  \S##7154532S\n\
  \S2##314#18S\n\
  \S971595#34S\n\
  \SSSSSSSSSSS"

day13 :: ECSolution
day13 = map (show . search) . parse (parseMaze `P.sepEndBy` P.newline)

parseMaze :: Parser (M.HashMap Point2d Int, [Point2d], Point2d)
parseMaze = do
  lines <- P.many1 (P.oneOf " #0123456789SE") `P.sepEndBy` P.newline
  let vs =
        [ ((i, j), v)
          | (i, line) <- zip [0 ..] lines,
            (j, v) <- zip [0 ..] line,
            v `notElem` " #"
        ]
      s = map fst $ filter ((== 'S') . snd) vs
      Just (e, _) = find ((== 'E') . snd) vs
  pure (M.fromList $ map (second toInt) $ filter ((/= 'S') . snd) vs, s, e)
  where
    toInt = \case
      'S' -> 0
      'E' -> 0
      x -> digitToInt x

search :: (M.HashMap Point2d Int, [Point2d], Point2d) -> Int
search (g, s, e) = r
  where
    Just (r, _) = aStar neighbours cost heur goal Nothing
    --
    heur = \case
      Nothing -> 0
      Just p -> manhattanDistance p e
    --
    goal :: Maybe Point2d -> Bool
    goal = \case
      Nothing -> False
      Just p -> p == e
    --
    cost :: Maybe Point2d -> Maybe Point2d -> Int
    cost Nothing _ = 0
    cost _ Nothing = 0
    cost (Just a) (Just b) = 1 + minimum [abs (h a - h b + i) | i <- [0, 10, -10]]
      where
        h x = M.lookupDefault 0 x g
    --
    neighbours :: Maybe Point2d -> [Maybe Point2d]
    neighbours p = map Just $ case p of
      Nothing -> s
      Just p -> filter (`M.member` g) $ point2dNeighbours p
