module Day07 (day07, day07TestInput) where

import Common
import Control.Monad (guard)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (sortBy)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day07TestInput :: String
day07TestInput =
  "A:+,-,=,=\n\
  \B:+,=,-,+\n\
  \C:=,-,+,+\n\
  \D:=,=,=,+\n\
  \\n\
  \S===\n\
  \=  =\n\
  \====\n\
  \\n\
  \A:+,-,=,=\n\
  \B:+,=,-,+\n\
  \C:=,-,+,+\n\
  \D:=,=,=,+\n\
  \\n\
  \S+===\n\
  \-   +\n\
  \=+=-+\n\
  \\n\
  \A:+,-\n\
  \\n\
  \S+===\n\
  \-   +\n\
  \=+=-+\n\
  \\n\
  \"

day07 :: ECSolution
day07 = parse do
  i1 <- uncurry (solveP12 1) <$> p
  i2 <- uncurry (solveP12 10) <$> p
  i3 <- solveP3 <$> p
  pure [i1, i2, i3]
  where
    p = do
      c <- pChariots
      t <- buildTrack <$> pTrack
      pure (t, c)
    pChariots = p `P.sepEndBy` P.newline <* P.newline
      where
        p = do
          x <- P.letter `P.manyTill` P.char ':'
          y <- P.oneOf "+-=" `P.sepBy` P.char ','
          pure (x, y)
    pTrack = (P.many (P.oneOf " S+-=") <* P.newline) `P.manyTill` P.newline

solveP12 :: Int -> String -> [(String, String)] -> String
solveP12 n t = concatMap fst . sortBy (flip compare `on` snd) . map (runChariot n t)

solveP3 :: (String, [(String, String)]) -> String
solveP3 (t, [c]) = show $ countTrue ((> v) . snd . f) xs
  where
    -- optimisation, 11 gives same result as 2024, but runs significantly quicker. see:
    -- https://www.reddit.com/r/everybodycodes/comments/1gpylzn/comment/lwurl8x/
    f = runChariot 11 t
    (_, v) = f c
    xs = ("",) <$> createPermutations [5, 3, 3]
    createPermutations :: [Int] -> [String]
    createPermutations [0, 0, 0] = [[]]
    createPermutations [a, b, c] = concat [a', b', c']
      where
        a' = if a > 0 then map ('+' :) $ createPermutations [pred a, b, c] else []
        b' = if b > 0 then map ('-' :) $ createPermutations [a, pred b, c] else []
        c' = if c > 0 then map ('=' :) $ createPermutations [a, b, pred c] else []

runChariot :: Int -> String -> (String, String) -> (String, Int)
runChariot n t (i, xs) = (i, sum . tail . scanl f 10 $ zip (concat $ replicate n t) $ cycle xs)
  where
    f acc x = max 0 $ g x acc
    g ('+', _) = succ
    g ('-', _) = pred
    g (_, x) = case x of
      '+' -> succ
      '-' -> pred
      '=' -> id

buildTrack :: [String] -> String
buildTrack t = go East (0, 0)
  where
    go :: Direction -> Point2d -> String
    go dir p = case grid M.!? p' of
      Nothing ->
        if q `M.member` grid
          then go (turn90 dir) p
          else go (turn270 dir) p
      Just 'S' -> "="
      Just x -> x : go dir p'
      where
        p' = moveOneStepInDir p dir
        q = moveOneStepInDir p (turn90 dir)
    grid :: M.HashMap Point2d Char
    grid = M.fromList do
      (i, line) <- zip [0 ..] t
      (j, v) <- zip [0 ..] line
      guard $ v /= ' '
      pure ((i, j), v)
