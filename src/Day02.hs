module Day02 (
  day02
) where

import Common
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type Input = [(String, Int)]
type Scores = M.Map String Int

day02 :: AOCSolution
day02 input = [show $ part1 i, show $ part2 i]
  where
    i = parseInput input

parseInput :: String -> Input
parseInput = M.toList . M.fromListWith (+) . map (,1) . lines

part1 :: Input -> Int
part1 [] = 0
part1 ((x, y):xs) = r + part1 xs
  where
    r = (s M.! x) * y
    s = scores getResult
    getResult :: (Char, Char) -> Int
    getResult (x, y) = r + y' + 1
      where
        getN = \case
          'A' -> 0
          'X' -> 0
          'B' -> 1
          'Y' -> 1
          'C' -> 2
          'Z' -> 2
        x' = getN x
        y' = getN y
        r = case mod (y' - x') 3 of
          0 -> 3 -- draw
          1 -> 6 -- win
          2 -> 0 -- lose


part2 :: Input -> Int
part2 [] = 0
part2 ((x, y):xs) = r + part2 xs
  where
    r = (s M.! x) * y
    s = scores getResult
    getResult :: (Char, Char) -> Int
    getResult = \case
      ('A', 'X') -> 3 -- lose with scissors
      ('A', 'Y') -> 4 -- draw with rock
      ('A', 'Z') -> 8 -- win with paper
      ('B', 'X') -> 1 -- lose with rock
      ('B', 'Y') -> 5 -- draw with paper
      ('B', 'Z') -> 9 -- win with scissors
      ('C', 'X') -> 2 -- lose with paper
      ('C', 'Y') -> 6 -- draw with scissors
      ('C', 'Z') -> 7 -- win with rock

scores :: ((Char, Char) -> Int) -> Scores
scores getResult = M.fromList $ zip (map (\(x, y) -> x:' ':[y]) hands) results
  where
    hands = (,) <$> ['A'..'C'] <*> ['X'..'Z']
    results = map getResult hands



