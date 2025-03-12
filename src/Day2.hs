module Day2 (partOne, partTwo) where

import qualified Data.Bifunctor as BF
import Text.Read (readEither)

newtype Error = ParseError String
  deriving (Show)

type Level = Int

type Report = [Level]

partOne :: String -> Either Error Int
partOne contents = length . filter isValidReport <$> parseInput contents
  where
    isValidReport xs = isValidReportWith isSafeIncrease xs || isValidReportWith isSafeDecrease xs

    isSafeIncrease x y = (x - y) > 0 && (x - y) < 4
    isSafeDecrease x y = isSafeIncrease y x

    isValidReportWith validator (x : y : xs) = validator x y && isValidReportWith validator (y : xs)
    isValidReportWith _ [_] = True
    isValidReportWith _ [] = False

partTwo :: String -> Either Error Int
partTwo contents = calculateScore <$> parseInput contents
  where
    calculateScore _ = 0

parseInput :: String -> Either Error [Report]
parseInput contents = mapM parseReport (lines contents)
  where
    parseReport :: String -> Either Error Report
    parseReport line = mapM parseLevel (words line)

    parseLevel :: String -> Either Error Level
    parseLevel word = BF.first (const $ ParseError word) (readEither word :: Either String Level)
