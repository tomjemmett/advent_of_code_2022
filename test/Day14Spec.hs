module Day14Spec ( spec ) where

import SpecHelper

testInput = "498,4 -> 498,6 -> 496,6\n\
            \503,4 -> 502,4 -> 502,9 -> 494,9"

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
      day14 testInput `shouldBe` ["24", "93"]
      
  it "Actual" $ do
    withFile "inputs/day14.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day14 actualInput `shouldBe` ["665","25434"])