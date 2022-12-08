module Day08Spec ( spec ) where

import SpecHelper

testInput = "30373\n\
            \25512\n\
            \65332\n\
            \33549\n\
            \35390"

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
      day08 testInput `shouldBe` ["21", "8"]
      
  it "Actual" $ do
    withFile "inputs/day08.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day08 actualInput `shouldBe` ["1801","209880"])