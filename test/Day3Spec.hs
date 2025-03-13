module Day3Spec (spec) where

import Day3 (partOne, partTwo)
import Test.Hspec

spec :: Spec
spec = do
  describe "PartOne" $ do
    it "Given a corrupted program, it extracts the valid operations and runs them" $ do
      let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      partOne input `shouldBe` (2 * 4 + 5 * 5 + 11 * 8 + 8 * 5)

  describe "PartTwo" $ do
    it "Given a corrupted program, it extracts the valid operations and runs them" $ do
      let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
      partTwo input `shouldBe` (2 * 4 + 8 * 5)
