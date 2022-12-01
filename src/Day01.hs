module Day01 (
  day01  
) where

import Common
import Data.List.Split (splitOn)

day01 :: AOCSolution
day01 input = show . sum <$> r
  where
    f = sortDesc . map (sum . linesRead) . splitOn "\n\n"
    r = take <$> [1, 3] <*> pure (f input)