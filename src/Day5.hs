module Day5 (ParseError, partOne, partTwo) where

import qualified Data.Bifunctor as BF
import Data.List (sortBy)
import Data.Set (Set, fromList, member)
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
  where
    isValidUpdate rules update = sortBy (compareByRules rules) update == update
    compareByRules rules x y = if member (x, y) rules then LT else GT
    middleElem update = update !! (length update `div` 2)

partTwo :: String -> Either ParseError Int
partTwo _ = Right 0

parseInput :: String -> Either ParseError (Set Rule, [Update])
parseInput content = case T.splitOn separator text of
  [left, right] -> do
    rules <- parseRules left
    updates <- parseUpdates right
    return (rules, updates)
  _ -> Left ParseInputError
  where
    text = T.pack content
    separator = T.pack "\n\n"

parseRules :: T.Text -> Either ParseError (Set Rule)
parseRules rules = fromList <$> mapM parseRule (T.lines rules)
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
