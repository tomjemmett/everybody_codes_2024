module Day05 (day05, day05TestInput) where

import Common
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as SQ
import Data.Vector qualified as V

type Dancers = V.Vector (SQ.Seq Int)

type Shouts = [String]

day05TestInput :: String
day05TestInput =
  "2 3 4 5\n\
  \3 4 5 2\n\
  \4 5 2 3\n\
  \5 2 3 4\n\
  \\n\
  \2 3 4 5\n\
  \6 7 8 9\n\
  \\n\
  \2 3 4 5\n\
  \6 7 8 9"

day05 :: ECSolution
day05 input = [part1 i1, part2 i2, part3 i3]
  where
    [i1, i2, i3] = parseInput input

part1 :: Dancers -> String
part1 = shout . snd . (!! 10) . iterate (execState dance) . (0,)

part2 :: Dancers -> String
part2 i = show $ go 0 M.empty states
  where
    states = map (shout . snd) $ iterate (execState dance) (0, i)
    go r m (x : xs) = if m' M.! x == 2024 then r * read x else go (succ r) m' xs
      where
        m' = M.insertWith (+) x 1 m

part3 :: Dancers -> String
part3 = evalState (go S.empty "") . (0,)
  where
    go :: S.HashSet String -> String -> State (Int, Dancers) String
    go s m = do
      i <- get
      if show i `S.member` s
        then pure m
        else do
          dance
          i' <- get
          let s' = S.insert (show i) s
          go s' (max m $ shout (snd i'))

parseInput :: String -> [Dancers]
parseInput = map fn . splitOn "\n\n"
  where
    fn = V.fromList . map SQ.fromList . transpose . map (map read . words) . lines

dance :: State (Int, Dancers) ()
dance = do
  (i, v) <- get
  let (x :<| s) = v V.! i
      i' = succ i `mod` V.length v
  put (i', v V.// [(i, s)])
  shuffle x

shuffle :: Int -> State (Int, Dancers) ()
shuffle c = do
  (i, v) <- get
  let s = v V.! i
      l = SQ.length s
      n = (c - 1) `mod` (2 * l)
      n' = if n >= l then (2 * l) - n else n
  put (i, v V.// [(i, SQ.insertAt n' c s)])

shout :: Dancers -> String
shout = concatMap (\(x :<| _) -> show x)
