module Day4Spec (spec) where

import Day4 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "works" $ do
      partOne input `shouldBe` 18

  describe "PartTwo" $ do
    it "works" $ do
      partTwo input `shouldBe` 9
  where
    input =
      "MMMSXXMASM\n\
      \MSAMXMSMSA\n\
      \AMXSXMAAMM\n\
      \MSAMASMSMX\n\
      \XMASAMXAMM\n\
      \XXAMMXXAMA\n\
      \SMSMSASXSS\n\
      \SAXAMASAAA\n\
      \MAMMMXMMMM\n\
      \MXMXAXMASX"
