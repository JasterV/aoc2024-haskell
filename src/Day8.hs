module Day8 (partOne, partTwo) where

import Data.List (nub)
import Data.List.Extra (pairs)
import qualified Data.Map.Lazy as Map
import Data.Matrix (Matrix, Position)
import qualified Data.Matrix as M

type Node = (Int, Int)

partOne :: String -> Int
partOne input = length $ filter (`M.isInBounds` grid) $ nub antiNodes
  where
    grid = M.buildMatrix (lines input)
    antiNodes = concatMap (uncurry getAntiNodes) $ nodePairs grid

    getAntiNodes x y = case distance x y of
      (0, 0) -> []
      d -> [add x d, substract y d]

partTwo :: String -> Int
partTwo input = length antiNodes
  where
    matrix = M.buildMatrix (lines input)
    combinations = nodePairs matrix
    antiNodes = nub $ concatMap (uncurry getAntiNodes) combinations

    getAntiNodes x y = case distance x y of
      (0, 0) -> []
      d -> computePointsAtDistance x d add ++ computePointsAtDistance y d substract

    computePointsAtDistance point dist f =
      let point' = f point dist
       in if M.isInBounds point matrix
            then point : computePointsAtDistance point' dist f
            else []

nodePairs :: Matrix Char -> [(Node, Node)]
nodePairs matrix =
  let noDotsMatrix = M.filter (/= '.') matrix
      pointGroups = Map.elems $ M.groupByWith (\(position, value) -> (value, position)) noDotsMatrix
   in concatMap pairs pointGroups

substract :: Position -> (Int, Int) -> (Int, Int)
substract (x, y) (dx, dy) = (x - dx, y - dy)

add :: Position -> (Int, Int) -> (Int, Int)
add (x, y) (dx, dy) = (x + dx, y + dy)

distance :: Position -> Position -> (Int, Int)
distance (xrow, xcol) (yrow, ycol) = (xrow - yrow, xcol - ycol)
