module Day18Spec (spec) where

import SpecHelper

testInput =
  "2,2,2\n\
  \1,2,2\n\
  \3,2,2\n\
  \2,1,2\n\
  \2,3,2\n\
  \2,2,1\n\
  \2,2,3\n\
  \2,2,4\n\
  \2,2,6\n\
  \1,2,5\n\
  \3,2,5\n\
  \2,1,5\n\
  \2,3,5"

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    day18 testInput `shouldBe` ["64", "58"]

  it "Actual" $ do
    withFile
      "inputs/day18.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day18 actualInput `shouldBe` ["4322", "2516"]
      )