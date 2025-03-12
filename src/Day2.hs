module Day2 (partOne, partTwo) where

import qualified Data.Bifunctor as BF
import Text.Read (readEither)

newtype Error = ParseError String
  deriving (Show)

type Level = Int

type Report = [Level]

partOne :: String -> Either Error Int
partOne contents = length . filter isSafeReport <$> parseInput contents

partTwo :: String -> Either Error Int
partTwo contents = length . filter isSafeReportWithTolerance <$> parseInput contents
  where
    isSafeReportWithTolerance xs = isSafeReport xs || aux [] xs
      where
        aux left [] = isSafeReport left
        aux left (x : xs') = isSafeReport (left ++ xs') || aux (left ++ [x]) xs'

isSafeReport :: Report -> Bool
isSafeReport xs = isSafeReportWith isSafeIncrease xs || isSafeReportWith isSafeDecrease xs
  where
    isSafeReportWith validator (x : y : xs') = validator x y && isSafeReportWith validator (y : xs')
    isSafeReportWith _ [_] = True
    isSafeReportWith _ [] = False
    isSafeIncrease x y = (x - y) > 0 && (x - y) < 4
    isSafeDecrease x y = isSafeIncrease y x

parseInput :: String -> Either Error [Report]
parseInput contents = mapM parseReport (lines contents)
  where
    parseReport :: String -> Either Error Report
    parseReport line = mapM parseLevel (words line)

    parseLevel :: String -> Either Error Level
    parseLevel word = BF.first (const $ ParseError word) (readEither word :: Either String Level)
