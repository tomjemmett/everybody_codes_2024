module Day11 (day11, day11TestInput) where

import Common
import Control.Monad.RWS
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Termites = M.HashMap String Int

type Rules = M.HashMap String [String]

day11TestInput :: String
day11TestInput =
  "\
  \A:B,C\n\
  \B:C,A\n\
  \C:A\n\
  \\n\
  \Z:Z\n\
  \\n\
  \A:B,C\n\
  \B:C,A,A\n\
  \C:A"

day11 :: ECSolution
day11 =
  map show . parse do
    is <- pLines `P.sepEndBy` P.newline
    pure $ zipWith (\f v -> f v) [part1, part2, part3] is
  where
    pLines = M.fromList <$> pLine `P.sepEndBy` P.newline
    pLine = do
      x <- P.many1 P.letter <* P.char ':'
      ys <- P.many1 P.letter `P.sepBy` P.char ','
      pure (x, ys)

part1 :: Rules -> Int
part1 = run 4 "A"

part2 :: Rules -> Int
part2 = run 10 "Z"

part3 :: Rules -> Int
part3 rules =
  let vs = map (flip (run 20) rules) $ M.keys rules
   in maximum vs - minimum vs

run :: Int -> String -> Rules -> Int
run i k rules = (!! pred i) $ snd $ execRWS step rules (M.singleton k 1)

step :: RWS Rules [Int] Termites ()
step = do
  xs <- concat <$> (gets M.toList >>= traverse (\(t, n) -> map (,n) . (M.! t) <$> ask))
  put $ M.fromListWith (+) xs
  tell [sum $ map snd xs]
  step
