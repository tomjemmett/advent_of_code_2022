module Day02Spec ( spec ) where

import SpecHelper

testInput = "A Y\n\
            \B X\n\
            \C Z"

spec :: Spec
spec = describe "Day 2" $ do
  it "Sample" $ do
      day02 testInput `shouldBe` ["15", "12"]
      
  it "Actual" $ do
    withFile "inputs/day02.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day02 actualInput `shouldBe` ["9651", "10560"])