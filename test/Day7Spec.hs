module Day7Spec (spec) where

import Day7 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` Right 3749
  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` Right 11387
  where
    input =
      "190: 10 19\n\
      \3267: 81 40 27\n\
      \83: 17 5\n\
      \156: 15 6\n\
      \7290: 6 8 6 15\n\
      \161011: 16 10 13\n\
      \192: 17 8 14\n\
      \21037: 9 7 18 13\n\
      \292: 11 6 16 20"
