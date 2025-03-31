module Day8 (partOne, partTwo) where

import Data.List (nub, subsequences)
import qualified Data.Map.Lazy as Map
import Data.Matrix (Position)
import qualified Data.Matrix as M
import Data.Maybe (mapMaybe)

type AntiNode = (Int, Int)

partOne :: String -> Int
partOne input = length $ filter (`M.isInBounds` matrix) $ nub $ concatMap (uncurry getAntiNodes) combinations
  where
    matrix = M.buildMatrix (lines input)
    noDotsMatrix = M.filter (/= '.') matrix
    pointGroups = Map.elems $ M.groupByWith (\(position, value) -> (value, position)) noDotsMatrix
    combinations = concatMap pairs pointGroups

partTwo :: String -> Int
partTwo input = undefined

pairs :: [a] -> [(a, a)]
pairs xs = mapMaybe parse $ subsequences xs
  where
    parse :: [a] -> Maybe (a, a)
    parse [x, y] = Just (x, y)
    parse _ = Nothing

getAntiNodes :: Position -> Position -> [AntiNode]
getAntiNodes (xrow, xcol) (yrow, ycol) = case distance of
  (0, 0) -> []
  (drow, dcol) -> [(xrow + drow, xcol + dcol), (yrow - drow, ycol - dcol)]
  where
    distance = (xrow - yrow, xcol - ycol)
