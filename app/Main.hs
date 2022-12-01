module Main where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Days

days = [(day01, "inputs/day01.txt"),
        (day02, "inputs/day02.txt"),
        (day03, "inputs/day03.txt"),
        (day04, "inputs/day04.txt"),
        (day05, "inputs/day05.txt"),
        (day06, "inputs/day06.txt"),
        (day07, "inputs/day07.txt"),
        (day08, "inputs/day08.txt"),
        (day09, "inputs/day09.txt"),
        (day10, "inputs/day10.txt"),
        (day11, "inputs/day11.txt"),
        (day12, "inputs/day12.txt"),
        (day13, "inputs/day13.txt"),
        (day14, "inputs/day14.txt"),
        (day15, "inputs/day15.txt"),
        (day16, "inputs/day16.txt"),
        (day17, "inputs/day17.txt"),
        (day18, "inputs/day18.txt"),
        (day19, "inputs/day19.txt"),
        (day20, "inputs/day20.txt"),
        (day21, "inputs/day21.txt"),
        (day22, "inputs/day22.txt"),
        (day23, "inputs/day23.txt"),
        (day24, "inputs/day24.txt"),
        (day25, "inputs/day25.txt")]

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ runDay [1..25]
    else runDay (read $ head args :: Int)
  
runDay :: Int -> IO ()
runDay day = do
  let (fn, file) = days !! pred day
  fileExists <- doesFileExist file
  if fileExists
    then do
      putStrLn $ replicate 80 '-'
      putStr $ "Day: " ++ show day
      putStrLn ""
      input <- readFile file
      putStr $ unlines $ fn input
    else do
      putStr ""