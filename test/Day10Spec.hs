module Day10Spec ( spec ) where

import SpecHelper

testInput = "addx 15\n\
            \addx -11\n\
            \addx 6\n\
            \addx -3\n\
            \addx 5\n\
            \addx -1\n\
            \addx -8\n\
            \addx 13\n\
            \addx 4\n\
            \noop\n\
            \addx -1\n\
            \addx 5\n\
            \addx -1\n\
            \addx 5\n\
            \addx -1\n\
            \addx 5\n\
            \addx -1\n\
            \addx 5\n\
            \addx -1\n\
            \addx -35\n\
            \addx 1\n\
            \addx 24\n\
            \addx -19\n\
            \addx 1\n\
            \addx 16\n\
            \addx -11\n\
            \noop\n\
            \noop\n\
            \addx 21\n\
            \addx -15\n\
            \noop\n\
            \noop\n\
            \addx -3\n\
            \addx 9\n\
            \addx 1\n\
            \addx -3\n\
            \addx 8\n\
            \addx 1\n\
            \addx 5\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx -36\n\
            \noop\n\
            \addx 1\n\
            \addx 7\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx 2\n\
            \addx 6\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx 1\n\
            \noop\n\
            \noop\n\
            \addx 7\n\
            \addx 1\n\
            \noop\n\
            \addx -13\n\
            \addx 13\n\
            \addx 7\n\
            \noop\n\
            \addx 1\n\
            \addx -33\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx 2\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx 8\n\
            \noop\n\
            \addx -1\n\
            \addx 2\n\
            \addx 1\n\
            \noop\n\
            \addx 17\n\
            \addx -9\n\
            \addx 1\n\
            \addx 1\n\
            \addx -3\n\
            \addx 11\n\
            \noop\n\
            \noop\n\
            \addx 1\n\
            \noop\n\
            \addx 1\n\
            \noop\n\
            \noop\n\
            \addx -13\n\
            \addx -19\n\
            \addx 1\n\
            \addx 3\n\
            \addx 26\n\
            \addx -30\n\
            \addx 12\n\
            \addx -1\n\
            \addx 3\n\
            \addx 1\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx -9\n\
            \addx 18\n\
            \addx 1\n\
            \addx 2\n\
            \noop\n\
            \noop\n\
            \addx 9\n\
            \noop\n\
            \noop\n\
            \noop\n\
            \addx -1\n\
            \addx 2\n\
            \addx -37\n\
            \addx 1\n\
            \addx 3\n\
            \noop\n\
            \addx 15\n\
            \addx -21\n\
            \addx 22\n\
            \addx -6\n\
            \addx 1\n\
            \noop\n\
            \addx 2\n\
            \addx 1\n\
            \noop\n\
            \addx -10\n\
            \noop\n\
            \noop\n\
            \addx 20\n\
            \addx 1\n\
            \addx 2\n\
            \addx 2\n\
            \addx -6\n\
            \addx -11\n\
            \noop\n\
            \noop\n\
            \noop"

sp2 = "##..##..##..##..##..##..##..##..##..##..\n\
      \###...###...###...###...###...###...###.\n\
      \####....####....####....####....####....\n\
      \#####.....#####.....#####.....#####.....\n\
      \######......######......######......####\n\
      \#######.......#######.......#######.....\n\
      \.\n"

ap2 = "####...##.#..#.###..#..#.#....###..####.\n\
      \#.......#.#..#.#..#.#..#.#....#..#....#.\n\
      \###.....#.#..#.###..#..#.#....#..#...#..\n\
      \#.......#.#..#.#..#.#..#.#....###...#...\n\
      \#....#..#.#..#.#..#.#..#.#....#.#..#....\n\
      \#.....##...##..###...##..####.#..#.####.\n\
      \.\n"

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
      day10 testInput `shouldBe` ["13140", sp2]
      
  it "Actual" $ do
    withFile "inputs/day10.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day10 actualInput `shouldBe` ["13060", ap2])