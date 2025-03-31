module Day8 (partOne, partTwo) where

import Data.List (nub)
import qualified Data.List.Extra as LE
import qualified Data.Map.Lazy as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Point (Point)
import qualified Data.Point as P

partOne :: String -> Int
partOne input = length $ filter (`M.isInBounds` matrix) $ nub antiNodes
  where
    matrix = M.buildMatrix (lines input)
    pairs = pairNodes matrix
    antiNodes = concatMap (uncurry getAntiNodes) pairs

    getAntiNodes x y =
      case P.distance x y of
        (0, 0) -> []
        d -> [P.add x d, P.substract y d]

partTwo :: String -> Int
partTwo input = length (nub antiNodes)
  where
    matrix = M.buildMatrix (lines input)
    pairs = pairNodes matrix
    antiNodes = concatMap (uncurry getAntiNodes) pairs

    getAntiNodes x y =
      let distance = P.distance x y
       in go x distance P.add ++ go y distance P.substract
      where
        go _ (0, 0) _ = []
        go point distance f =
          let point' = f point distance
           in if M.isInBounds point matrix
                then point : go point' distance f
                else []

pairNodes :: Matrix Char -> [(Point, Point)]
pairNodes matrix =
  let noDotsMatrix = M.filter (/= '.') matrix
      pointGroups = Map.elems $ M.groupByWith (\(position, value) -> (value, position)) noDotsMatrix
   in concatMap LE.pairs pointGroups
