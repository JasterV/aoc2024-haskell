module Day6Spec (spec) where

import Day6 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` Right 41

  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` Right 6
  where
    input =
     "....#.....\n\
     \.........#\n\
     \..........\n\
     \..#.......\n\
     \.......#..\n\
     \..........\n\
     \.#..^.....\n\
     \........#.\n\
     \#.........\n\
     \......#..."
