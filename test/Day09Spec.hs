module Day09Spec ( spec ) where

import SpecHelper

testInput1 = "R 4\n\
             \U 4\n\
             \L 3\n\
             \D 1\n\
             \R 4\n\
             \D 1\n\
             \L 5\n\
             \R 2"

testInput2 = "R 5\n\
             \U 8\n\
             \L 8\n\
             \D 3\n\
             \R 17\n\
             \D 10\n\
             \L 25\n\
             \U 20"

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
      day09 testInput1 `shouldBe` ["13", "1"]
      day09 testInput2 `shouldBe` ["88", "36"]
      
  it "Actual" $ do
    withFile "inputs/day09.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day09 actualInput `shouldBe` ["6494","2691"])