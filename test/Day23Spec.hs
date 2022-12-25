module Day23Spec (spec) where

import SpecHelper

testInput =
  "....#..\n\
  \..###.#\n\
  \#...#.#\n\
  \.#...##\n\
  \#.###..\n\
  \##.#.##\n\
  \.#..#.."

spec :: Spec
spec = describe "Day 23" $ do
  it "Sample" $ do
    day23 testInput `shouldBe` ["110", "20"]

  it "Actual" $ do
    withFile
      "inputs/day23.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day23 actualInput `shouldBe` ["3947", "1012"]
      )