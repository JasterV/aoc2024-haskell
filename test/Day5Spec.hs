module Day5Spec (spec) where

import Day5 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` Right 143

  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` Right 123
  where
    input =
      "47|53\n\
      \97|13\n\
      \97|61\n\
      \97|47\n\
      \75|29\n\
      \61|13\n\
      \75|53\n\
      \29|13\n\
      \97|29\n\
      \53|29\n\
      \61|53\n\
      \97|53\n\
      \61|29\n\
      \47|13\n\
      \75|47\n\
      \97|75\n\
      \47|61\n\
      \75|61\n\
      \47|29\n\
      \75|13\n\
      \53|13\n\
      \\n\
      \75,47,61,53,29\n\
      \97,61,53,29,13\n\
      \75,29,13\n\
      \75,97,47,61,53\n\
      \61,13,29\n\
      \97,13,75,29,47"
