module Day10 (partOne, partTwo) where

import Data.Char (digitToInt)
import Data.List (nub)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Point (Point)

partOne :: String -> Int
partOne input = sum $ map (length . nub . trailEnds matrix) heads
  where
    matrix = parseMatrix input
    heads = trailHeads matrix

partTwo :: String -> Int
partTwo input = sum $ map (length . trailEnds matrix) heads
  where
    matrix = parseMatrix input
    heads = trailHeads matrix

trailHeads :: Matrix Int -> [Point]
trailHeads matrix = M.points $ M.filter (== 0) matrix

parseMatrix :: String -> Matrix Int
parseMatrix = M.buildMatrix . map (map digitToInt) . lines

trailEnds :: Matrix Int -> Point -> [Point]
trailEnds matrix x = go [x] 0
  where
    go :: [Point] -> Int -> [Point]
    go [] _ = []
    go front step
      | step < 9 =
          let front' = concatMap (getCandidates (1 + step)) front
           in go front' (step + 1)
      | otherwise = front

    getCandidates :: Int -> Point -> [Point]
    getCandidates value (row, col) =
      let positions = [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
          values = map (`M.lookup` matrix) positions
       in map fst
            $ filter
              ( \(_, mValue) -> case mValue of
                  Nothing -> False
                  Just v -> v == value
              )
            $ zip positions values
