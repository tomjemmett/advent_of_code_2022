{-# LANGUAGE ViewPatterns #-}
module Day10 (
  day10
) where


import Common
import Data.List.Split (chunksOf)

day10 :: AOCSolution
day10 input = [part1, part2] <*> pure (parseInput input)

part1 :: [Int] -> String
part1 = show . sum . f 20 . drop 19
  where
    f _ [] = []
    f i (x:xs) = (x * i):g i xs
    g i = f (i + 40) . drop 39

part2 :: [Int] -> String
part2 = unlines . chunksOf 40 . go . zip [0..]
  where
    go [] = []
    go ((x, y):xs) = p:go xs
      where
        p = if abs ((x `mod` 40) - y) <= 1
          then '#'
          else '.'

parseInput :: String -> [Int]
parseInput = (1:) . parseLine 1 . lines
  where
    parseLine :: Int -> [String] -> [Int]
    parseLine _ [] = []
    parseLine regx ("noop":xs) = regx:parseLine regx xs
    parseLine regx (((regx+) . read . drop 5 -> n):xs) = regx:n:parseLine n xs