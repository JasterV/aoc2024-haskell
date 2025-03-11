module Day1
  ( partOne,
    partTwo,
  )
where

import Control.Exception (try)
import qualified Data.Bifunctor as Bifunctor
import Data.Either
import Data.List (sort)
import Text.Read (readEither)
import Prelude hiding (id)

type LocationID = Int

data ParseError
  = ParseLocationError String String
  | ParseLineError String
  deriving (Show)

data Error
  = ReadFileError String
  | ParseInputError [ParseError]
  deriving (Show)

partOne :: String -> IO (Either Error Int)
partOne filepath = do
  result <- tryReadFile filepath
  case result of
    Left readError -> return (Left readError)
    Right contents -> return (calculateScore <$> parseInput contents)
  where
    -- To calculate the overall score we just need to
    -- sort the lists and calculate the distances between each element
    calculateScore (left, right) =
      sum $ zipWith distance (sort left) (sort right)
    distance a b = abs (a - b)

partTwo :: String -> IO (Either Error Int)
partTwo filepath = do
  result <- tryReadFile filepath
  case result of
    Left readError -> return (Left readError)
    Right contents -> return (calculateScore <$> parseInput contents)
  where
    -- To calculate the overall score we need to sum all the similarity scores
    -- of each element from the left list applied to the right list
    calculateScore (left, right) = sum $ map (`similarity` right) left
    -- The similarity score of a location id is equal to itself
    -- multiplied by the number of times it appears on the list
    similarity id ids = id * count id ids
    count x = length . filter (== x)

parseInput :: String -> Either Error ([LocationID], [LocationID])
parseInput input = Bifunctor.first ParseInputError $ unzip <$> parseLines (lines input)

parseLines :: [String] -> Either [ParseError] [(LocationID, LocationID)]
parseLines xs =
  if null errors
    then Right parsedLines
    else Left errors
  where
    errors = lefts $ map (parseWords . words) xs
    parsedLines = rights $ map (parseWords . words) xs

parseWords :: [String] -> Either ParseError (LocationID, LocationID)
parseWords [left, right] = do
  leftLocation <- parseLeft
  rightLocation <- parseRight
  return (leftLocation, rightLocation)
  where
    parseLeft :: Either ParseError LocationID
    parseLeft =
      Bifunctor.first
        (ParseLocationError left)
        (readEither left :: Either String LocationID)

    parseRight :: Either ParseError LocationID
    parseRight =
      Bifunctor.first
        (ParseLocationError right)
        (readEither right :: Either String LocationID)

-- If the line does not contain exactly two words,
-- it is considered an error
parseWords xs' = Left $ ParseLineError (unwords xs')

tryReadFile :: String -> IO (Either Error String)
tryReadFile filepath = Bifunctor.first (const $ ReadFileError filepath) <$> io
  where
    io :: IO (Either IOError String)
    io = try (readFile filepath)
