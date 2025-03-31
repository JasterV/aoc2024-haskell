module Day7 (partOne, partTwo) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

type Operand = Int

type Operator = (Int -> Int -> Int)

type Equation = (Int, [Operand])

data ParseEquationError = ParseEquationError | ParseOperandError | ParseResultError
  deriving (Show, Eq)

partOne :: String -> Either ParseEquationError Int
partOne input = calibrate operators <$> parseEquations input
  where
    operators :: [Operator]
    operators = [(+), (*)]

partTwo :: String -> Either ParseEquationError Int
partTwo input = calibrate operators <$> parseEquations input
  where
    operators :: [Operator]
    operators = [(+), (*), concatNums]

    concatNums :: Int -> Int -> Int
    concatNums x y = read (show x <> show y)

calibrate :: [Operator] -> [Equation] -> Int
calibrate operators = sum . map fst . filter canBeSolved
  where
    canBeSolved (result, operands) = elem result $ solvePossibleEquations operands

    solvePossibleEquations [] = []
    solvePossibleEquations [_] = []
    solvePossibleEquations (x : y : xs) =
      let solutions = map (\f -> f x y) operators
       in case xs of
            [] -> solutions
            operands -> concatMap (solvePossibleEquations . (: operands)) solutions

parseEquations :: String -> Either ParseEquationError [Equation]
parseEquations input =
  mapM parseEquation (T.lines (T.pack input))
  where
    parseEquation :: Text -> Either ParseEquationError Equation
    parseEquation line =
      case T.splitOn (T.pack ": ") line of
        [left, right] -> do
          let right' = T.splitOn (T.pack " ") right
          operands <- mapM (parseInt ParseOperandError) right'
          result <- parseInt ParseResultError left
          return (result, operands)
        _ -> Left ParseEquationError

    parseInt :: e -> Text -> Either e Int
    parseInt e x = case readMaybe (T.unpack x) of
      Nothing -> Left e
      Just number -> Right number
