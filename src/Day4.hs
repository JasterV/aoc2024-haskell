module Day4 (partOne, partTwo) where

import Data.List (sort, transpose)
import qualified Data.Map.Lazy as Map
import qualified Data.Matrix as M

partOne :: String -> Int
partOne input = sum $ map countXMAS allLines
  where
    countXMAS :: String -> Int
    countXMAS ('X' : xs@('M' : 'A' : 'S' : _)) = 1 + countXMAS xs
    countXMAS ('S' : xs@('A' : 'M' : 'X' : _)) = 1 + countXMAS xs
    countXMAS (_ : xs) = countXMAS xs
    countXMAS [] = 0

    horizontalLines = lines input
    verticalLines = transpose horizontalLines
    matrix = M.buildMatrix horizontalLines
    positiveDiagonals = Map.elems $ M.groupByWith (\((row, col), value) -> (row + col, value)) matrix
    negativeDiagonals = Map.elems $ M.groupByWith (\((row, col), value) -> (row - col, value)) matrix
    allLines = horizontalLines ++ verticalLines ++ positiveDiagonals ++ negativeDiagonals

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
