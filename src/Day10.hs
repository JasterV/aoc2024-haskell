module Day10 (partOne, partTwo) where

import Data.Char (digitToInt)
import Data.List (intersect)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Point (Point)

partOne :: String -> Int
partOne input = length $ filter (uncurry (isReachable matrix)) candidates
  where
    matrix = parseMatrix input
    trailHeads = M.points $ M.filter (== 0) matrix
    trailEnds = M.points $ M.filter (== 9) matrix
    candidates = concatMap (zip trailHeads . repeat) trailEnds

isReachable :: Matrix Int -> Point -> Point -> Bool
isReachable matrix x y = go (getCandidates 1 x) [y] 1
  where
    go :: [Point] -> [Point] -> Int -> Bool
    go _ _ 9 = False
    go leftFront rightFront step =
      let leftFront' = concatMap (getCandidates (1 + step)) leftFront
          rightFront' = concatMap (getCandidates (9 - step)) rightFront
       in not (null (leftFront `intersect` rightFront)) || go leftFront' rightFront' (step + 1)

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

partTwo :: String -> Int
partTwo input = 0

parseMatrix :: String -> Matrix Int
parseMatrix = M.buildMatrix . map (map digitToInt) . lines
