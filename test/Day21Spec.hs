module Day21Spec (spec) where

import SpecHelper

testInput =
  "root: pppw + sjmn\n\
  \dbpl: 5\n\
  \cczh: sllz + lgvd\n\
  \zczc: 2\n\
  \ptdq: humn - dvpt\n\
  \dvpt: 3\n\
  \lfqf: 4\n\
  \humn: 5\n\
  \ljgn: 2\n\
  \sjmn: drzm * dbpl\n\
  \sllz: 4\n\
  \pppw: cczh / lfqf\n\
  \lgvd: ljgn * ptdq\n\
  \drzm: hmdt - zczc\n\
  \hmdt: 32"

spec :: Spec
spec = describe "Day 21" $ do
  it "Sample" $ do
    day21 testInput `shouldBe` ["152", "301"]

  it "Actual" $ do
    withFile
      "inputs/day21.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day21 actualInput `shouldBe` ["81075092088442", "3349136384441"]
      )