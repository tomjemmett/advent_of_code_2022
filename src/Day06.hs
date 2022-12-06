module Day06 (
  day06
) where

import Common
import Data.List (nub)

day06 :: AOCSolution
day06 input = go . f <$> [4, 14] <*> pure input
  where
    f x = (x, x)

go :: (Int, Int) -> String -> String
go (i, n) x
  | (length $ nub $ take n x) == n = show i
  | otherwise = go (succ i, n) $ drop 1 x