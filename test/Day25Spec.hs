module Day25Spec (spec) where

import SpecHelper

testInput =
  "1=-0-2\n\
  \12111\n\
  \2=0=\n\
  \21\n\
  \2=01\n\
  \111\n\
  \20012\n\
  \112\n\
  \1=-1=\n\
  \1-12\n\
  \12\n\
  \1=\n\
  \122"

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["2=-1=0"]

  it "Actual" $ do
    withFile
      "inputs/day25.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day25 actualInput `shouldBe` ["2-==10--=-0101==1201"]
      )