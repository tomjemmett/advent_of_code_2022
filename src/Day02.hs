module Day02 (
  day02
) where

import Common
import Data.Char (ord)
import qualified Data.Map as M

type Pair = (Int, Int)
type Input = [(Pair, Int)]
type Part = Pair -> Int

day02 :: AOCSolution
day02 input = show <$> (go <$> [part1, part2] <*> pure (parseInput input))

parseInput :: String -> Input
parseInput = M.toList . M.fromListWith (+) . map f . lines
  where
    f (x:_:y:[]) = ((ord x - 65, ord y - 88), 1)

go :: Part -> Input -> Int
go _ [] = 0
go fn ((x, y):xs) = (fn x) * y + go fn xs

part1 :: Part
part2 :: Part
part1 (x1, x2) = ((x2 - x1 + 1) `mod` 3) * 3 + (x2 + 1)
part2 (x1, x2) = ((x1 + x2 + 2) `mod` 3) + 1 + (x2 * 3) 
