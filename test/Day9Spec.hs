module Day9Spec (spec) where

import Day9 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` 1928
  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` 0
  where
    input = "2333133121414131402"
