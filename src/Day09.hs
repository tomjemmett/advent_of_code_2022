{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
module Day09 (
  day09
) where

import Common
import qualified Data.HashSet as H
import Control.Monad.State

type Visited = H.HashSet Point2d

day09 :: AOCSolution
day09 input = go <$> [2, 10] <*> parseInput input

parseInput :: Applicative f => String -> f [(Char, Int)]
parseInput = pure . foldr f [] . lines
  where
    f :: [Char] -> [(Char, Int)] -> [(Char, Int)]
    f (m:' ':(read -> s)) p = (m, s):p

go :: Int -> [(Char, Int)] -> String
go n = show . H.size . f (replicate n (0, 0), mempty)
  where
    f :: ([Point2d], Visited) -> [(Char, Int)] -> Visited
    f (_, v) [] = v
    f (p, v) (m:ms) = f (runState (move m p) v) ms

move :: (Char, Int) -> [Point2d] -> State Visited [Point2d]
move (_, 0) p = state (p,)
move (m, n) (hp:tp) = do
  v <- get
  let
    hp' = moveH hp m
    p' = scanl moveT hp' tp
  put $ H.insert (last p') v
  move (m, pred n) p'

moveH :: Point2d -> Char -> Point2d
moveH (hx, hy) = \case
  'R' -> (hx, succ hy)
  'L' -> (hx, pred hy)
  'U' -> (succ hx, hy)
  'D' -> (pred hx, hy)

moveT :: Point2d -> Point2d -> Point2d
moveT (hx, hy) (tx, ty)
  | dx <= 1 && dy <= 1   = (tx, ty)
  | dx == 2 && dy == 0   = (f hx tx, ty)
  | dx == 0 && dy == 2   = (tx, f hy ty)
  | otherwise            = (f hx tx, f hy ty)
  where
    f a b = if a < b then pred b else succ b
    dx = abs (hx - tx)
    dy = abs (hy - ty)
