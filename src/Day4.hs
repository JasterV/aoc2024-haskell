module Day4 (partOne, partTwo) where

import Data.List (transpose)
import qualified Day4.Matrix as M

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
partTwo input =
  let matrix = M.buildMatrix (lines input)
   in length $ filter (isXMAS . fst) $ filter ((== 'a') . snd) $ M.toList matrix
  where
    isXMAS = undefined
