module Day13Spec ( spec ) where

import SpecHelper

testInput = "[1,1,3,1,1]\n\
            \[1,1,5,1,1]\n\
            \\n\
            \[[1],[2,3,4]]\n\
            \[[1],4]\n\
            \\n\
            \[9]\n\
            \[[8,7,6]]\n\
            \\n\
            \[[4,4],4,4]\n\
            \[[4,4],4,4,4]\n\
            \\n\
            \[7,7,7,7]\n\
            \[7,7,7]\n\
            \\n\
            \[]\n\
            \[3]\n\
            \\n\
            \[[[]]]\n\
            \[[]]\n\
            \\n\
            \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
            \[1,[2,[3,[4,[5,6,0]]]],8,9]"

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
      day13 testInput `shouldBe` ["13", "140"]
      
  it "Actual" $ do
    withFile "inputs/day13.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day13 actualInput `shouldBe` ["6369","25800"])