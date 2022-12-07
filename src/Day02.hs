{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02
  ( day02,
  )
where

import Common
import Data.Char (ord)
import qualified Data.Map as M

type Pair = (Int, Int)

type Score = Int

type Input = [(Pair, Score)]

type Part = Pair -> Score

-- run the go function for both part1 and part2 with our parsed input, and
-- show the results as a list of strings
day02 :: AOCSolution
day02 input = show <$> (go <$> [part1, part2] <*> pure (parseInput input))

-- we parse the input turning each line into a pair of integers (0 for rock,
-- 1 for scissors, 2 for paper), then counts how many times each pair appears
parseInput :: String -> Input
parseInput = M.toList . M.fromListWith (+) . map f . lines
  where
    f [x, _, y] = ((ord x - 65, ord y - 88), 1)

-- go is a recursive function that iterates over each "line" of our input
-- it will run the "part" function for the pair (x) that has been played that
-- round and multiply the results by the amount of times that pair is played (y)
go :: Part -> Input -> Int
go _ [] = 0
go fn ((x, y) : xs) = fn x * y + go fn xs

-- our part functions that the pair that is played in a round and returns the
-- score for that pair
part1 :: Part
part2 :: Part
part1 (x1, x2) = ((x2 - x1 + 1) `mod` 3) * 3 + (x2 + 1)

part2 (x1, x2) = ((x1 + x2 + 2) `mod` 3) + 1 + (x2 * 3)
