module Day06 (day06, day06TestInput) where

import Common
import Control.Monad.Writer
  ( MonadWriter (tell),
    Writer,
    execWriter,
    forM_,
  )
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Tree = M.HashMap String [String]

day06TestInput :: String
day06TestInput =
  "RR:A,B,C\n\
  \A:D,E\n\
  \B:F,@\n\
  \C:G,H\n\
  \D:@\n\
  \E:@\n\
  \F:@\n\
  \G:@\n\
  \H:@\n\
  \\n\
  \RR:A,B,C\n\
  \A:D,E\n\
  \B:F,@\n\
  \C:G,H\n\
  \D:@\n\
  \E:@\n\
  \F:@\n\
  \G:@\n\
  \H:@\n\
  \\n\
  \RR:A,B,C\n\
  \A:D,E\n\
  \B:F,@\n\
  \C:G,H\n\
  \D:@\n\
  \E:@\n\
  \F:@\n\
  \G:@\n\
  \H:@\n\
  \\n\
  \"

day06 :: ECSolution
day06 = parse do
  p1 <- concat . solve <$> parseTree
  p2 <- map head . solve <$> parseTree
  p3 <- map head . solve <$> parseTree
  pure [p1, p2, p3]

parseTree :: Parser Tree
parseTree = M.fromList <$> (p `P.sepEndBy` P.newline <* P.newline)
  where
    p = do
      x <- P.letter `P.manyTill` P.char ':'
      y <- P.many1 (P.noneOf ",\n") `P.sepBy` P.char ','
      pure (x, y)

solve :: Tree -> [String]
solve m = concat $ head $ M.elems $ M.filter ((== 1) . length) r'
  where
    r = execWriter (go [] "RR")
    r' = foldr1 (M.unionWith (++)) $ zipWith M.singleton (map length r) (map (: []) r)
    go :: [String] -> String -> Writer [[String]] ()
    go s = \case
      "@" -> tell [reverse $ "@" : s]
      "ANT" -> pure ()
      "BUG" -> pure ()
      k -> forM_ (M.lookupDefault [] k m) (go (k : s))
