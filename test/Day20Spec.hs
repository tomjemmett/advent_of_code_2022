module Day20Spec (spec) where

import SpecHelper

testInput = "1\n2\n-3\n3\n-2\n0\n4"

spec :: Spec
spec = describe "Day 20" $ do
  it "Sample" $ do
    day20 testInput `shouldBe` ["3", "1623178306"]

  it "Actual" $ do
    withFile
      "inputs/day20.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day20 actualInput `shouldBe` ["4578", "2159638736133"]
      )