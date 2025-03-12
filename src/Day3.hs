module Day3 (partOne, partTwo) where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA

-- TODO: Refactor to parse operations to a type and then be executed by a "execute" function

partOne :: String -> Int
partOne text = sum $ map (uncurry (*)) $ mapMaybe parseOperation matches
  where
    matches = getAllTextMatches (text =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

partTwo :: String -> Int
partTwo text =
  sum $
    map (uncurry (*)) $
      mapMaybe parseOperation $
        filterDisabledOperations matches
  where
    matches = getAllTextMatches (text =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)")

data ExecutionState = Enabled | Disabled

filterDisabledOperations :: [String] -> [String]
filterDisabledOperations operations = aux Enabled operations []
  where
    aux _ [] acc = acc
    aux _ ("don't()" : xs) acc = aux Disabled xs acc
    aux _ ("do()" : xs) acc = aux Enabled xs acc
    aux Enabled (op : xs) acc = aux Enabled xs (op : acc)
    aux Disabled (_ : xs) acc = aux Disabled xs acc

parseOperation :: String -> Maybe (Int, Int)
parseOperation x =
  case getAllTextMatches (x =~ "[0-9]{1,3}") of
    [left, right] -> do
      leftOp <- readMaybe left :: Maybe Int
      rightOp <- readMaybe right :: Maybe Int
      return (leftOp, rightOp)
    _ -> Nothing
