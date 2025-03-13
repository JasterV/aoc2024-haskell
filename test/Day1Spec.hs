module Day1Spec (spec) where

import Day1 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "Given two valid lists it returns the total distance" $ do
      let expectedResult = 11
      partOne testInput `shouldBe` Right expectedResult

  describe "PartTwo" $ do
    it "Given two valid lists it returns their similarity score" $ do
      partTwo testInput `shouldBe` Right 31
  where
    testInput =
      "3   4\n\
      \4   3\n\
      \2   5\n\
      \1   3\n\
      \3   9\n\
      \3   3"
