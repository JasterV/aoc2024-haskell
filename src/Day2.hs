module Day2 (partOne, partTwo) where

data Error = ParseError String
  deriving (Show)

partOne :: String -> Either Error Int
partOne contents = calculateScore <$> parseInput contents
  where
    calculateScore _ = 0

partTwo :: String -> Either Error Int
partTwo contents = calculateScore <$> parseInput contents
  where
    calculateScore _ = 0

parseInput :: String -> Either Error [Int]
parseInput _ = Right []

