module Day08 (
  day08
) where

import Common
import Data.Bifunctor (bimap)
import Data.Either (isLeft)
import Data.List (transpose, splitAt)
import qualified Data.Vector as V

day08 :: AOCSolution
day08 input = show <$> ([part1, part2] <*> parseInput input)

parseInput :: Applicative f => String -> f [[Either Int Int]]
parseInput input = pure $ countTrees trees <$> ((,) <$> [1..x] <*> [1..y])
  where
    trees = parseGrid2d input
    x = length trees
    y = length $ trees V.! 1

countTrees :: Grid2d -> Point2d -> [Either Int Int]
countTrees trees p = f <$> cij
  where
    cij = uncurry bimap <$> [(pred, id), (succ, id), (id, pred), (id, succ)]
    t = lookupInGrid2d trees p
    f c = iterate 0 trees t (c p) c
    iterate :: Int -> Grid2d -> Int -> Point2d -> (Point2d -> Point2d) -> Either Int Int
    iterate n trees t p cij = case (t >) <$> lookupInGrid2d' trees p of
      Nothing   -> Left n
      Just True -> iterate (succ n) trees t (cij p) cij
      _         -> Right $ succ n

part1 :: [[Either Int Int]] -> Int
part1 = countTrue id . map (any isLeft)

part2 :: [[Either Int Int]] -> Int
part2 = maximum . map (product . map (either id id))