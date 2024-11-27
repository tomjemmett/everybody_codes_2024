module Day02 (day02, day02TestInput) where

import Common
import Data.List (nub, sortBy, splitAt, transpose)
import Data.List.Split (splitOn)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day02TestInput :: String
day02TestInput =
  "WORDS:THE,OWE,MES,ROD,HER\n\
  \\n\
  \AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE\n\
  \\n\
  \\n\
  \WORDS:THE,OWE,MES,ROD,HER,QAQ\n\
  \\n\
  \AWAKEN THE POWE ADORNED WITH THE FLAMES BRIGHT IRE\n\
  \THE FLAME SHIELDED THE HEART OF THE KINGS\n\
  \POWE PO WER P OWE R\n\
  \THERE IS THE END\n\
  \QAQAQ\n\
  \\n\
  \\n\
  \WORDS:THE,OWE,MES,ROD,RODEO\n\
  \\n\
  \HELWORLT\n\
  \ENIGWDXL\n\
  \TRODEOAL"

day02 :: ECSolution
day02 input = show <$> [part1 i1, part2 i2, part3 i3]
  where
    [i1, i2, i3] = parseInput input

parseInput :: String -> [([String], [String])]
parseInput = map (parse p) . splitOn "\n\n\n"
  where
    p = do
      P.string "WORDS:"
      w <- commaSeparated (P.many P.letter)
      P.newline
      P.newline
      t <- P.many (P.noneOf "\n") `P.sepBy` P.newline
      pure (w, t)

part1 :: ([String], [String]) -> Int
part1 (words, target) = p1 $ unwords target
  where
    p1 [] = 0
    p1 target = f words target
      where
        f [] (_ : xs) = p1 xs
        f (w : ws) xs@(_ : xs')
          | x == w = succ $ p1 xs'
          | otherwise = f ws xs
          where
            x = take (length w) xs

part2 :: ([String], [String]) -> Int
part2 (w, target) = length . nub . concat $ p2 (unwords target) 0 []
  where
    words = sortDesc $ w ++ map reverse w
    p2 [] _ res = res
    p2 target i res = f words target
      where
        f [] (_ : xs) = p2 xs (succ i) res
        f (w : ws) xs@(_ : xs')
          | x == w = p2 xs' (succ i) res'
          | otherwise = f ws xs
          where
            lw = length w
            x = take lw xs
            res' = [i .. (i + lw - 1)] : res

part3 :: ([String], [String]) -> Int
part3 (words, target) = length . nub . concat $ p3
  where
    p3 = do
      let target_h =
            -- convert all the targets into a list where each line of the targets
            -- is split into it's own list, with each character being represented as it's
            -- cartesian coordinate inside the grid, and the character
            -- replicate the horizontal line so we can handle "wrapping" from left to right
            map (concat . replicate 2) $
              [ [((i, j), t) | (j, t) <- zip [0 ..] ts]
                | (i, ts) <- zip [0 ..] target
              ]
          -- do the same for the vertical view of the grid, no need to handle "wrapping"
          target_v =
            [ [ ((i, j), t) | (i, t) <- zip [0 ..] ts
              ]
              | (j, ts) <- zip [0 ..] $ transpose target
            ]
          -- combine these into our new targets
          targets = target_h ++ target_v
      -- for each word
      w <- words
      -- for each target
      t <- targets
      -- find the length of the word and get the reverse version of the word
      let lw = length w
          m = reverse w
      -- for the current target line, get all of the substrings which are of the length of the word
      (i, j) <- fn t lw
      -- if the word is found in the target, return the found coordinates
      pure $ if (i == w) || (i == m) then j else []
    fn :: [(a, b)] -> Int -> [([b], [a])]
    fn [] _ = []
    fn t@(_ : ts) i = r : fn ts i
      where
        x = take i t
        r = (map snd x, map fst x)
