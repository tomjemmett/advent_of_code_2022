module Day22Spec (spec) where

import SpecHelper

testInput =
  "        ...#\n\
  \        .#..\n\
  \        #...\n\
  \        ....\n\
  \...#.......#\n\
  \........#...\n\
  \..#....#....\n\
  \..........#.\n\
  \        ...#....\n\
  \        .....#..\n\
  \        .#......\n\
  \        ......#.\n\
  \\n\
  \10R5L5R10L4R5L5"

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 testInput `shouldBe` ["6032"]

  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` ["31568", "36540"]
      )