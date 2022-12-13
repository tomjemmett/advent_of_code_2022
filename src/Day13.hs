module Day13 (
  day13
) where

import Common
import Data.List.Split (splitOn)

import Data.Maybe (fromJust)
import Data.List (sort, elemIndex)

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

data Item = Val Int | List [Item] deriving (Show, Eq)

instance Ord Item where
  compare (Val l) (Val r) = compare l r
  compare (List ls) (List rs) = cmp ls rs
    where
      cmp :: [Item] -> [Item] -> Ordering
      cmp [] [] = EQ
      cmp [] _  = LT
      cmp _  [] = GT
      cmp (l:ls) (r:rs) = let c = compare l r in
        if c == EQ
          then cmp ls rs
          else c
  compare v@(Val _) l@(List _) = compare (List [v]) l
  compare l@(List _) v@(Val _) = compare l (List [v])

day13 :: AOCSolution
day13 input = show <$> [part1 i, part2 i]
  where
    i = parseInput input

part1 :: [[Item]] -> Int
part1 = f 1 . map (uncurry compare . tuplify2)
  where
    f :: Int -> [Ordering] -> Int
    f n x
      | null x    = 0
      | x1 == LT  = n + next
      | otherwise = 0 + next
      where
        (x1:xs) = x
        next = f (succ n) xs

part2 :: [[Item]] -> Int
part2 i = product $ succ . fromJust . (`elemIndex` i') <$> d
  where
    d = [List [List [Val x]] | x <- [2, 6]]
    i' = sort $ d ++ concat i

parseInput :: String -> [[Item]]
parseInput = map2 (parse' parseItem id) . map (splitOn "\n") . splitOn "\n\n"

parseItem :: Parser Item
parseItem =
  P.choice [
    Val <$> number,
    List <$> (P.char '[' *> parseItem `P.sepBy` P.char ',' <* P.char ']')
  ]
