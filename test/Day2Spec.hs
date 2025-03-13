module Day2Spec (spec) where

import Day2 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "Given a list of reports, it returns how many are valid" $ do
      partOne testInput `shouldBe` Right 2

  describe "PartTwo" $ do
    it "Given a list of reports, it returns how many are valid" $ do
      partTwo testInput `shouldBe` Right 4
  where
    testInput =
      "7 6 4 2 1\n\
      \1 2 7 8 9\n\
      \9 7 6 2 1\n\
      \1 3 2 4 5\n\
      \8 6 4 4 1\n\
      \1 3 6 7 9"
