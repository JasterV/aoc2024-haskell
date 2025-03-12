module Day3 (partOne, partTwo) where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA

-- Part One

partOne :: String -> Int
partOne text = sum $ map (uncurry (*)) $ mapMaybe parseOperators operations
  where
    operations :: [String]
    operations = getAllTextMatches (text =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

-- Part Two

data ExecutionState = Enabled | Disabled

partTwo :: String -> Int
partTwo text =
  sum $
    map (uncurry (*)) $
      mapMaybe parseOperators $
        filterDisabledOperations operations
  where
    operations :: [String]
    operations = getAllTextMatches (text =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)")

    filterDisabledOperations :: [String] -> [String]
    filterDisabledOperations ops = aux ops [] Enabled
      where
        aux [] acc _ = acc
        aux ("don't()" : ops') acc _ = aux ops' acc Disabled
        aux ("do()" : ops') acc _ = aux ops' acc Enabled
        aux (op : ops') acc Enabled = aux ops' (op : acc) Enabled
        aux (_ : ops') acc Disabled = aux ops' acc Disabled

-- Shared

parseOperators :: String -> Maybe (Int, Int)
parseOperators x =
  case matches of
    [left, right] -> do
      leftOp <- readMaybe left :: Maybe Int
      rightOp <- readMaybe right :: Maybe Int
      return (leftOp, rightOp)
    _ -> Nothing
  where
    matches = getAllTextMatches (x =~ "[0-9]{1,3}") :: [String]
