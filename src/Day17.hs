{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day17
  ( day17,
  )
where

import Common
import Control.Monad.State
import Data.Bits
import Data.HashMap.Strict qualified as M
import Data.Vector.Unboxed qualified as V
import Data.Word

type Shape = [Word8]

type Grid = [Word8]

type Wind = V.Vector Char

day17 :: AOCSolution
day17 input = show . go i M.empty 0 0 0 [0b1111111] <$> [2022, 1000000000000]
  where
    i = parseInput input

-- make sure to just use the first line of input
parseInput :: String -> Wind
parseInput = V.fromList . head . lines

shapes :: [Shape]
shapes =
  [ [0b0011110],
    [0b0001000, 0b0011100, 0b0001000],
    [0b0000100, 0b0000100, 0b0011100],
    [0b0010000, 0b0010000, 0b0010000, 0b0010000],
    [0b0011000, 0b0011000]
  ]

wind :: Grid -> Char -> Shape -> Shape
wind g w s
  | any (`testBit` n) s = s
  | any (/= 0) $ zipWith (.&.) g s' = s
  | otherwise = s'
  where
    (n, f) = if w == '<' then (6, shiftL) else (0, shiftR)
    s' = map (`f` 1) s

fall :: Grid -> Wind -> Int -> Shape -> (Int, [Word8])
fall g ws wix s
  | all (== 0) $ zipWith (.&.) s g = fall g ws (succ wix) $ 0 : s'
  | otherwise = (wix, insertShape (tail s) g)
  where
    w = ws V.! (wix `mod` V.length ws)
    s' = wind g w s

insertShape :: Shape -> Grid -> Grid
insertShape shape grid = dropWhile (== 0) $ go shape grid
  where
    go [] gs = gs
    go (r : rs) (g : gs) = (r .|. g) : go rs gs

go ::
  Wind ->
  M.HashMap (Int, Int, Shape) (Int, Int) ->
  Int ->
  Int ->
  Int ->
  Grid ->
  Int ->
  Int
go w seen h i k g lim
  | i >= lim = h + length g - 1
  | otherwise = case M.lookup state seen of
    Nothing -> go w seen' h (i + 1) k' g' lim
    Just (sn, height) ->
      let hd = length grid - 1 - height
          id = i - sn
          cy = (lim - i) `div` id
       in go w seen' (h + cy * hd) (i + 1 + cy * id) k' g' lim
  where
    grid = replicate (3 + length (shapes !! (i `mod` 5))) 0 ++ g
    (k', g') = fall grid w k $ shapes !! (i `mod` 5)
    state = (k' `mod` V.length w, i `mod` 5, take 30 grid)
    seen' = M.insert state (i, length grid - 1) seen