module Day01 (
  day01  
) where

import Common
import Data.List.Split (splitOn)

day01 :: AOCSolution
day01 input = show <$> [p1, p2]
  where
    i = sortDesc . map sum . map (map (read::String->Int)) . map lines . splitOn "\n\n" $ input
    p1 = head i
    p2 = sum $ take 3 i

-- check that each item in the list is less than the nth next item
-- for part 1, n = 1 (the next item)
-- for part 2, n = 3: sum (a, b, c) < sum (b, c, d) iff a < d
countIncrements :: Int -> [Int] -> String
countIncrements n x = show $ countTrue id $ zipWith (<) x (drop n x)