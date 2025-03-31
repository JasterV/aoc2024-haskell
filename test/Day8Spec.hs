module Day8Spec (spec) where

import Day8 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` 14
  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` 34
  where
    input =
      "............\n\
      \........0...\n\
      \.....0......\n\
      \.......0....\n\
      \....0.......\n\
      \......A.....\n\
      \............\n\
      \............\n\
      \........A...\n\
      \.........A..\n\
      \............\n\
      \............"
