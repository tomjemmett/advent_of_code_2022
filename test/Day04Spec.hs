module Day04Spec ( spec ) where

import SpecHelper

testInput = "2-4,6-8\n\
            \2-3,4-5\n\
            \5-7,7-9\n\
            \2-8,3-7\n\
            \6-6,4-6\n\
            \2-6,4-8"

spec :: Spec
spec = describe "Day 4" $ do
  return ()
  it "Sample" $ do
      day04 testInput `shouldBe` ["2", "4"]
      
  it "Actual" $ do
    withFile "inputs/day04.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day04 actualInput `shouldBe` ["602","891"])