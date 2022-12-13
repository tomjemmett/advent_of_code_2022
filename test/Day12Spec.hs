module Day12Spec ( spec ) where

import SpecHelper

testInput = "Sabqponm\n\
            \abcryxxl\n\
            \accszExk\n\
            \acctuvwj\n\
            \abdefghi"

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
      day12 testInput `shouldBe` ["31", "29"]
      
  it "Actual" $ do
    withFile "inputs/day12.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day12 actualInput `shouldBe` ["380","375"])