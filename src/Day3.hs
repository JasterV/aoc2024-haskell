module Day3 (partOne, partTwo) where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA

partOne :: String -> Int
partOne text = programValue $ foldl runInstruction initProgram $ parseInstructions matches
  where
    matches = getAllTextMatches (text =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

partTwo :: String -> Int
partTwo text = programValue $ foldl runInstruction initProgram $ parseInstructions matches
  where
    matches = getAllTextMatches (text =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)")

-- Program

data ProgramState = Enabled | Disabled

data Program = Program ProgramState Int

data Instruction = Do | Dont | Mul Int Int

initProgram :: Program
initProgram = Program Enabled 0

programValue :: Program -> Int
programValue (Program _ value) = value

runInstruction :: Program -> Instruction -> Program
runInstruction (Program _ value) Do = Program Enabled value
runInstruction (Program _ value) Dont = Program Disabled value
runInstruction (Program Disabled value) _ = Program Disabled value
runInstruction (Program Enabled value) (Mul x y) = Program Enabled (value + (x * y))

parseInstructions :: [String] -> [Instruction]
parseInstructions = mapMaybe parseInstruction
  where
    parseInstruction "do()" = Just Do
    parseInstruction "don't()" = Just Dont
    parseInstruction x =
      case getAllTextMatches (x =~ "[0-9]{1,3}") of
        [left, right] -> do
          leftOp <- readMaybe left :: Maybe Int
          rightOp <- readMaybe right :: Maybe Int
          return (Mul leftOp rightOp)
        _ -> Nothing
