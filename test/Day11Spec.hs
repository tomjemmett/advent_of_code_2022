module Day11Spec ( spec ) where

import SpecHelper

testInput = "Monkey 0:\n\
            \  Starting items: 79, 98\n\
            \  Operation: new = old * 19\n\
            \  Test: divisible by 23\n\
            \    If true: throw to monkey 2\n\
            \    If false: throw to monkey 3\n\
            \\n\
            \Monkey 1:\n\
            \  Starting items: 54, 65, 75, 74\n\
            \  Operation: new = old + 6\n\
            \  Test: divisible by 19\n\
            \    If true: throw to monkey 2\n\
            \    If false: throw to monkey 0\n\
            \\n\
            \Monkey 2:\n\
            \  Starting items: 79, 60, 97\n\
            \  Operation: new = old * old\n\
            \  Test: divisible by 13\n\
            \    If true: throw to monkey 1\n\
            \    If false: throw to monkey 3\n\
            \\n\
            \Monkey 3:\n\
            \  Starting items: 74\n\
            \  Operation: new = old + 3\n\
            \  Test: divisible by 17\n\
            \    If true: throw to monkey 0\n\
            \    If false: throw to monkey 1"
            
spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
      day11 testInput `shouldBe` ["10605", "2713310158"]
      
  it "Actual" $ do
    withFile "inputs/day11.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day11 actualInput `shouldBe` ["56120", "24389045529"])