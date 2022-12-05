module Day05Spec ( spec ) where

import SpecHelper

testInput = "    [D]    \n\
            \[N] [C]    \n\
            \[Z] [M] [P]\n\
            \ 1   2   3 \n\
            \\n\
            \move 1 from 2 to 1\n\
            \move 3 from 1 to 3\n\
            \move 2 from 2 to 1\n\
            \move 1 from 1 to 2"

spec :: Spec
spec = describe "Day 5" $ do
  return ()
  it "Sample" $ do
      day05 testInput `shouldBe` ["CMZ", "MCD"]
      
  it "Actual" $ do
    withFile "inputs/day05.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day05 actualInput `shouldBe` ["WHTLRMZRC","GMPMLWNMG"])