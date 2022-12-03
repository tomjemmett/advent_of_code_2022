module Day03 (
  day03
) where

import Common
import Data.Char (ord)
import Data.List (intersect, splitAt)

day03 :: AOCSolution
day03 input = go <$> [p1, p2] <*> pure input
  where
    p1 = (halveLines, 2)
    p2 = (id, 3)

go :: (([String] -> [String]), Int) -> String -> String
go (fn, n) = show . sum . map getPriority . findCommon n . fn . lines

-- takes a list of lines, halves each line, and returns a new list of lines
-- (double the length of the input)
halveLines :: [String] -> [String]
halveLines [] = []
halveLines (x:xs) = a:b:(halveLines xs)
  where
    n = (length x) `div` 2
    (a,b) = splitAt n x

-- gets the priority of the first character in a string, ignoring anything else
getPriority :: String -> Int
getPriority (c:_) = ord c - if c < 'a' then 38 else 96
 
-- find the common characters between 
findCommon :: Int -> [String] -> [String]
findCommon _ [] = []
findCommon n xs = (foldl1 intersect x):(findCommon n y)
  where
    (x,y) = splitAt n xs