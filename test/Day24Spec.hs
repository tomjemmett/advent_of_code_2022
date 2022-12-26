module Day24Spec (spec) where

import SpecHelper

testInput =
  "#.######\n\
  \#>>.<^<#\n\
  \#.<..<<#\n\
  \#>v.><>#\n\
  \#<^v^^>#\n\
  \######.#"

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 testInput `shouldBe` ["18", "54"]

  it "Actual" $ do
    withFile
      "inputs/day24.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day24 actualInput `shouldBe` ["253", "794"]
      )