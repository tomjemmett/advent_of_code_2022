{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day15 where --(
--   day15
-- ) where

import Common
import Data.Bifunctor (second)
import Data.List (sort, find)
import Data.Maybe (mapMaybe, fromJust)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

testInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
            \Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
            \Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
            \Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
            \Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
            \Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
            \Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
            \Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
            \Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
            \Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
            \Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
            \Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
            \Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
            \Sensor at x=20, y=1: closest beacon is at x=15, y=3"

day15 :: AOCSolution
day15 input = show <$> ([part1 lineNum, part2 maxCoord] <*> pure i)
  where
    isTestInput = length input == 737
    lineNum  = if isTestInput then 10 else 2000000
    maxCoord = lineNum * 2
    i = parseInput input

parseInput :: String -> [(Point2d, Point2d, Int)]
parseInput = map (parse' parseLine id) . lines
  where
    parseLine = do
      P.string "Sensor at x="
      sx <- number
      P.string ", y="
      sy <- number
      P.string ": closest beacon is at x="
      bx <-number
      P.string ", y="
      by <- number
      let
        (s, b)    = ((sx, sy), (bx, by))
        dist      = manhattanDistance s b
      pure (s, b, dist)

intervalOnLine :: Int -> (Point2d, Point2d, Int) -> Maybe (Int, Int)
intervalOnLine l ((x, y), _, d) = if abs d' > d
  then Nothing
  else Just (x - d + abs d', x + d - abs d')
  where
    d' = l - y

intervalsIntersect :: (Int, Int) -> (Int, Int) -> Bool
intervalsIntersect (a1, a2) (b1, b2) = or
  [ a1 <= b1 && a2 >= b1
  , a1 <= b1 && a2 >= b2
  , a1 >= b1 && a2 <= b2
  , a1 >= b1 && a2 >= b1
  ]

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce [b] = [b]
reduce (a@(a1,a2):b@(b1,b2):xs)
  | intervalsIntersect a b = reduce ((min a1 b1, max a2 b2):xs)
  | otherwise              = a:reduce (b:xs)
  where
    i = intervalsIntersect a b

go :: Int -> [(Point2d, Point2d, Int)] -> [(Int, Int)]
go line = reduce . sort . mapMaybe (intervalOnLine line)

part1 :: Int -> [(Point2d, Point2d, Int)] -> Int
part1 line = abs . uncurry (-) . head . go line

part2 :: Int -> [(Point2d, Point2d, Int)] -> Int
part2 to i = uncurry (+)
  $ second ((* 4000000) . succ . snd . head)
  $ fromJust
  $ find ((== 2) . length . snd)
  $ (\line -> (line, go line i)) <$> [0..to]
