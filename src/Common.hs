{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Common where

import Data.Foldable (toList)
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

import qualified Data.Vector as V
import Data.Vector ((!), (!?))

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type AOCSolution = String -> [String]
type Point2d = (Int, Int)
type Point3d = (Int, Int, Int)
type Grid2d = V.Vector (V.Vector Int)

countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

linesRead :: Read a => String -> [a]
linesRead = map read . lines

linesWords :: String -> [[String]]
linesWords = map words . lines

numbersStringToInt :: (String -> [String]) -> String -> [Int]
numbersStringToInt split = map read . split

wordSeparatedInts :: String -> [Int]
wordSeparatedInts = numbersStringToInt words

commaSeparatedInts :: String -> [Int]
commaSeparatedInts = numbersStringToInt (splitOn ",")

bitStringToInt :: String -> Int
bitStringToInt = bitsToInt . map digitToInt

bitsToInt :: [Int] -> Int
bitsToInt = sum . zipWith (*) (map (2^) [0..]) . reverse

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

parse :: Parser a -> String -> Either P.ParseError a
parse = flip P.parse ""

-- assumes that the parser always succeeds, it applies a function f to the Right value from the parser
parse' :: Parser a -> (a -> b) -> String -> b
parse' p f s = case parse p s of Right x -> f x

number :: Parser Int
number = read <$> P.many1 P.digit

numbers :: String -> Parser [Int]
numbers s = number `P.sepBy` (P.many1 . P.oneOf) s

parseGrid2d :: String -> Grid2d
parseGrid2d = V.fromList . map V.fromList . map2 digitToInt . lines

lookupInGrid2d :: Grid2d -> Point2d -> Int
lookupInGrid2d i (r, c) = fromMaybe 9 $ i !? r >>= flip (!?) c

point2dNeighbours :: Point2d -> [Point2d]
point2dNeighbours (r, c) = [(pred r, c), (succ r, c), (r, pred c), (r, succ c)]

point2dNeighboursDiags :: Point2d -> [Point2d]
point2dNeighboursDiags (r, c) = point2dNeighbours (r, c) ++ [
  (pred r, pred c), (pred r, succ c),
  (succ r, pred c), (succ r, succ c)]

sortPoint2d :: [Point2d] -> [Point2d]
sortPoint2d = sortBy comparePoint2d

comparePoint2d :: Point2d -> Point2d -> Ordering
comparePoint2d (a, b) (c, d)
      | a < c     = LT
      | a > c     = GT
      | b < d     = LT
      | b > d     = GT
      | otherwise = EQ

median :: Ord a => [a] -> [a]
median x = if odd lx then [xs !! hl] else [xs !! pred hl, xs !! hl]
  where
    xs = sort x
    lx = length xs
    hl = lx `div` 2

tuplify2 :: [a] -> (a, a)
tuplify2 [a,b] = (a,b)

tuplify3 :: [a] -> (a, a, a)
tuplify3 [a,b,c] = (a,b,c)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)