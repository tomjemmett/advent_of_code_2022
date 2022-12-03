module Day03Spec ( spec ) where

import SpecHelper

testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
            \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
            \PmmdzqPrVvPwwTWBwg\n\
            \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
            \ttgJtRGJQctTZtZT\n\
            \CrZsJsPPZsGzwwsLwLmpwMDw"

spec :: Spec
spec = describe "Day 3" $ do
  return ()
  it "Sample" $ do
      day03 testInput `shouldBe` ["157", "70"]
      
  it "Actual" $ do
    withFile "inputs/day03.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day03 actualInput `shouldBe` ["7746", "2604"])