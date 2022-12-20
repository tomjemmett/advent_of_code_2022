module Day19Spec (spec) where

import SpecHelper

testInput =
  "Blueprint 1: \
  \Each ore robot costs 4 ore. \
  \Each clay robot costs 2 ore. \
  \Each obsidian robot costs 3 ore and 14 clay. \
  \Each geode robot costs 2 ore and 7 obsidian.\n\
  \\
  \Blueprint 2: \
  \Each ore robot costs 2 ore. \
  \Each clay robot costs 3 ore. \
  \Each obsidian robot costs 3 ore and 8 clay. \
  \Each geode robot costs 3 ore and 12 obsidian."

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    -- day19 testInput `shouldBe` ["33", "3472"]
    day19 testInput `shouldBe` ["33", "3348"]

  it "Actual" $ do
    withFile
      "inputs/day19.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          -- day19 actualInput `shouldBe` ["1599", "14112"]
          day19 actualInput `shouldBe` ["1570", "9936"]
      )