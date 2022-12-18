module Day18
  ( day18,
  )
where

import Common
import Control.Monad (guard)
import Data.HashSet qualified as H

day18 :: AOCSolution
day18 input = show <$> (countTrue <$> [p1, p2] <*> pure n)
  where
    i = parseInput input
    n = concatMap point3dNeighbours $ H.toList i
    p1 = not . flip H.member i
    p2 = flip H.member (H.difference (search i [(-1, -1, -1)]) i)

parseInput :: String -> H.HashSet Point3d
parseInput = H.fromList . map (tuplify3 . commaSeparatedInts) . lines

search :: H.HashSet Point3d -> [Point3d] -> H.HashSet Point3d
search i [] = i
search i (x : xs) = search (H.insert x i) (n ++ xs)
  where
    n = do
      y <- point3dNeighbours x
      guard $ not $ H.member y i
      guard $ all (isBetween (-1, 25)) $ untuplify3 y
      pure y
