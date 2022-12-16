module Day15
  ( day15,
  )
where

import Common
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day15 :: AOCSolution
day15 input = show <$> ([part1 lineNum, part2 maxCoord] <*> pure i)
  where
    isTestInput = length input == 737
    lineNum = if isTestInput then 10 else 2000000
    maxCoord = lineNum * 2
    i = parseInput input

parseInput :: String -> [(Point2d, Int)]
parseInput = parse' (parseLine `P.sepEndBy` P.newline) id
  where
    parseLine = do
      sx <- P.string "Sensor at x=" *> number
      sy <- P.string ", y=" *> number
      bx <- P.string ": closest beacon is at x=" *> number
      by <- P.string ", y=" *> number
      let (s, b) = ((sx, sy), (bx, by))
          dist = manhattanDistance s b
      pure (s, dist)

intervalOnLine :: Int -> (Point2d, Int) -> Maybe Interval
intervalOnLine l ((x, y), d) =
  if abs d' > d
    then Nothing
    else Just (x - d', x + d')
  where
    d' = d - abs (l - y)

go :: Int -> [(Point2d, Int)] -> [Interval]
go line = reduceIntervals . sort . mapMaybe (intervalOnLine line)

part1 :: Int -> [(Point2d, Int)] -> Int
part1 line = abs . uncurry (-) . head . go line

part2 :: Int -> [(Point2d, Int)] -> Int
part2 to = p2 [0 .. to]
  where
    p2 [] _ = 0
    p2 (l : ls) i =
      if length r == 2
        then (+ l) . (* 4000000) . succ . snd . head $ r
        else p2 ls i
      where
        r = go l i
