module Day01Spec ( spec ) where

import SpecHelper

testInput = "1000\n\
            \2000\n\
            \3000\n\
            \\n\
            \4000\n\
            \\n\
            \5000\n\
            \6000\n\
            \\n\
            \7000\n\
            \8000\n\
            \9000\n\
            \\n\
            \10000"

spec :: Spec
spec = describe "Day 01" $ do
  it "Sample" $ do
      day01 testInput `shouldBe` ["24000", "45000"]
      
  it "Actual" $ do
    withFile "inputs/day01.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day01 actualInput `shouldBe` ["72017","212520"])