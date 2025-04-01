module Day10 (partOne, partTwo) where

import Data.Char (digitToInt)
import Data.List (intersect)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Point (Point)

partOne :: String -> Int
partOne input = length $ filter ((> 0) . uncurry (rating matrix)) candidates
  where
    matrix = parseMatrix input
    candidates = possibleTrails matrix

partTwo :: String -> Int
partTwo input = sum $ map (uncurry (rating matrix)) candidates
  where
    matrix = parseMatrix input
    candidates = possibleTrails matrix

rating :: Matrix Int -> Point -> Point -> Int
rating matrix x y = go (getCandidates 1 x) [y] 1
  where
    go :: [Point] -> [Point] -> Int -> Int
    go leftFront rightFront step
      | step > 5 = 0
      | step < 5 =
          let leftFront' = concatMap (getCandidates (1 + step)) leftFront
              rightFront' = concatMap (getCandidates (9 - step)) rightFront
           in go leftFront' rightFront' (step + 1)
      | otherwise =
          let intersectionA = intersect leftFront rightFront
              intersectionB = intersect rightFront leftFront
              intersection = if length intersectionA > length intersectionB then intersectionA else intersectionB
           in length intersection

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

possibleTrails :: Matrix Int -> [(Point, Point)]
possibleTrails matrix =
  let trailHeads = M.points $ M.filter (== 0) matrix
      trailEnds = M.points $ M.filter (== 9) matrix
   in concatMap (zip trailHeads . repeat) trailEnds

parseMatrix :: String -> Matrix Int
parseMatrix = M.buildMatrix . map (map digitToInt) . lines
