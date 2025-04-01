module Day10Spec (spec) where

import Day10 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` 36
  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` 81
  where
    input =
      "89010123\n\
      \78121874\n\
      \87430965\n\
      \96549874\n\
      \45678903\n\
      \32019012\n\
      \01329801\n\
      \10456732\n"
