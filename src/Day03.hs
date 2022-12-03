module Day03 (
  day03,
  part1,
  part2
) where

import Common
import Data.Char (ord)
import Data.List (nub, sort)

day03 :: AOCSolution
day03 input = show . sum <$> ([part1, part2] <*> pure (lines input))

part1 :: [String] -> [Int]
part2 :: [String] -> [Int]
part1 = map (getPriority . head . findCommon 2 . splitLine)
part2 = map getPriority . findCommon 3 . map (nub . sort)

splitLine :: String -> [String]
splitLine line = (nub . sort) <$> ([take, drop] <*> pure n <*> pure line)
  where
    n = (length line) `div` 2

getPriority :: String -> Int
getPriority (c:_) = ord c - if c < 'a' then 38 else 96

findCommon :: Int -> [String] -> [String]
findCommon _ [] = []
findCommon n xs = (foldl1 f $ take n xs):(findCommon n $ drop n xs)
  where
    f:: String -> String -> String
    f _ [] = []
    f [] _ = []
    f (x:xs) (y:ys)
      | x == y = x:f xs ys
      | x >  y    = f (x:xs) ys
      | otherwise = f xs (y:ys)