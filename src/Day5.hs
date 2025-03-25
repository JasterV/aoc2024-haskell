module Day5 (ParseError, partOne, partTwo) where

import qualified Data.Bifunctor as BF
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Text.Read (decimal)

data ParseError = ParseIntError String | ParseRuleError T.Text | ParseInputError
  deriving (Show, Eq)

type Rule = (Int, Int)

type Update = [Int]

partOne :: String -> Either ParseError Int
partOne input = do
  (rules, updates) <- parseInput input
  return $ sum $ map middleElem $ filter (isValidUpdate rules) updates

partTwo :: String -> Either ParseError Int
partTwo input = do
  (rules, updates) <- parseInput input
  return $ sum $ map (middleElem . sortUpdate rules) $ filter (not . isValidUpdate rules) updates

isValidUpdate :: HashSet Rule -> Update -> Bool
isValidUpdate rules update = sortUpdate rules update == update

sortUpdate :: HashSet Rule -> Update -> Update
sortUpdate rules = sortBy compareByRules
  where
    compareByRules x y = if HashSet.member (x, y) rules then LT else GT

middleElem :: Update -> Int
middleElem update = update !! (length update `div` 2)

parseInput :: String -> Either ParseError (HashSet Rule, [Update])
parseInput content = case T.splitOn separator text of
  [left, right] -> do
    rules <- parseRules left
    updates <- parseUpdates right
    return (rules, updates)
  _ -> Left ParseInputError
  where
    text = T.pack content
    separator = T.pack "\n\n"

parseRules :: T.Text -> Either ParseError (HashSet Rule)
parseRules rules = HashSet.fromList <$> mapM parseRule (T.lines rules)
  where
    parseRule rule = case T.split (== '|') rule of
      [left, right] -> do
        leftNum <- parseInt left
        rightNum <- parseInt right
        return (leftNum, rightNum)
      _ -> Left (ParseRuleError rule)

parseUpdates :: T.Text -> Either ParseError [Update]
parseUpdates updates = mapM parseUpdate $ T.lines updates
  where
    parseUpdate = mapM parseInt . T.split (== ',')

parseInt :: T.Text -> Either ParseError Int
parseInt v = fst <$> BF.first ParseIntError (decimal v)
