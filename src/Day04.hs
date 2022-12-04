module Day04 (
  day04
) where

import Common
import Data.List (sort, group)
import Data.List.Split (splitOn)

data Overlaps = None | Full | Partial deriving (Show, Eq, Ord)

day04 :: AOCSolution
day04 input = show <$> scanl1 (+) i
  where
    i = tail $ map length $ group $ sort $ overlaps <$> parseInput input

parseInput :: String -> [[[Int]]]
parseInput = map splitLine . lines
  where
    splitRange = map read . splitOn "-"
    splitLine = map splitRange . splitOn ","

overlaps :: [[Int]] -> Overlaps
overlaps [[a,b],[c,d]]
  | a <= c && b >= d = Full
  | c <= a && d >= b = Full
  | a <= d && b >= c = Partial
  | d <= a && c >= b = Partial
  | otherwise        = None
