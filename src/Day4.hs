module Day4 (partOne, partTwo) where

import Data.List (sort, transpose)
import qualified Data.Matrix as M

partOne :: String -> Int
partOne input =
  let horizontalLines = lines input
      verticalLines = transpose horizontalLines
      matrix = M.buildMatrix horizontalLines
      positiveDiagonals = M.groupWith (uncurry (+)) matrix
      negativeDiagonals = M.groupWith (uncurry (-)) matrix
      allLines = horizontalLines ++ verticalLines ++ positiveDiagonals ++ negativeDiagonals
   in sum $ map countXMAS allLines
  where
    countXMAS :: String -> Int
    countXMAS ('X' : xs@('M' : 'A' : 'S' : _)) = 1 + countXMAS xs
    countXMAS ('S' : xs@('A' : 'M' : 'X' : _)) = 1 + countXMAS xs
    countXMAS (_ : xs) = countXMAS xs
    countXMAS [] = 0

partTwo :: String -> Int
partTwo input = M.size (M.filterWithKey isXMAS matrix)
  where
    matrix = M.buildMatrix (lines input)

    isXMAS (col, row) 'A' =
      let firstDiagonal = M.lookupMultiple [(col - 1, row - 1), (col + 1, row + 1)] matrix
          secondDiagonal = M.lookupMultiple [(col - 1, row + 1), (col + 1, row - 1)] matrix
       in sort firstDiagonal == "MS"
            && sort secondDiagonal == "MS"
    isXMAS _ _ = False
