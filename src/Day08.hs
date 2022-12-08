module Day08 (
  day08
) where

import Common
import qualified Data.Vector as V
import Data.List (transpose, splitAt)

type Trees = ([[Int]], [[Int]])
type Part = Int -> Trees -> [(Int, Int)] -> Int

day08 :: AOCSolution
day08 input = go <$> [part1, part2] <*> parseInput input

parseInput :: Applicative f => String -> f Trees
parseInput input = pure (transpose i, i)
  where
    i = V.toList $ V.toList <$> parseGrid2d input

go :: Part -> Trees -> String
go f trees = show $ f v trees [(i, j) | i <- [1..(x - 2)], j <- [1..(y - 2)]]
  where
    x = length $ fst trees
    y = length $ fst trees !! 1
    v = x * 2 + (y - 2) * 2

part1 :: Part
part1 v (a, b) = (v+) . countTrue id . map f
  where
    f (i, j) = any (t >) $ maximum <$> [l, r, u, d]
      where
        (l, t:r) = splitAt j $ a !! i
        (u, _:d) = splitAt i $ b !! j

part2 :: Part
part2 _ (a, b) = maximum . map f
  where
    f (i, j) = product $ countTrees 0 t <$> [reverse l, r, reverse u, d]
      where
        (l, t:r) = splitAt j $ a !! i
        (u, _:d) = splitAt i $ b !! j
        countTrees :: Int -> Int -> [Int] -> Int
        countTrees n _ [] = n
        countTrees n t (x:xs) = if x < t
          then countTrees (succ n) t xs
          else succ n