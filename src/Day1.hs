module Day1 (
  partOne,
  partTwo,
) where

import Control.Exception
import Data.Data
import Data.List (sort)

type LocationID = Int

data ParseError = ParseError
  deriving (Show, Typeable)

instance Exception ParseError

partOne :: IO Int
partOne = do
  contents <- readFile "input/day1.txt"
  let (left, right) = parseInput contents
  let score = sum $ zipWith distance (sort left) (sort right)
  pure score
 where
  distance a b = abs (a - b)

partTwo :: IO Int
partTwo = do
  contents <- readFile "input/day1.txt"
  let (left, right) = parseInput contents
  let score = sum $ map (`similarity` right) left
  pure score
 where
  similarity :: LocationID -> [LocationID] -> Int
  similarity x xs = x * count x xs

  count :: LocationID -> [LocationID] -> Int
  count x = length . filter (== x)

parseInput :: String -> ([LocationID], [LocationID])
parseInput = unzip . map (parseWords . words) . lines
 where
  parseWords [left, right] = (read left :: LocationID, read right :: LocationID)
  parseWords _ = throw ParseError
