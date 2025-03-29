module Day6 (partOne, partTwo) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Matrix (Matrix, Position)
import qualified Data.Matrix as M
import Data.Maybe
import Prelude hiding (Left, Right)

data Error = StartingPositionNotFound
  deriving (Show)

partOne :: String -> Maybe Int
partOne input = length <$> predictGuardsRoute (parseLabMap input)

partTwo :: String -> Int
partTwo _input = 0

-- Laboratory Map
--
type LabMap = Matrix Char

type Direction = Char

type Guard = (Position, Direction)

type Visited = HashSet (Position, Direction)

parseLabMap :: String -> LabMap
parseLabMap = M.buildMatrix . lines

findGuard :: LabMap -> Maybe Guard
findGuard matrix =
  let mUp = M.lookupValue '^' matrix
      mDown = M.lookupValue 'v' matrix
      mRight = M.lookupValue '>' matrix
      mLeft = M.lookupValue '<' matrix
   in case [mUp, mDown, mRight, mLeft] of
        [Just pos, _, _, _] -> Just (pos, '^')
        [_, Just pos, _, _] -> Just (pos, 'v')
        [_, _, Just pos, _] -> Just (pos, '>')
        [_, _, _, Just pos] -> Just (pos, '<')
        _ -> Nothing

predictGuardsRoute :: LabMap -> Maybe [Position]
predictGuardsRoute labMap = do
  guard <- findGuard labMap
  return (go guard HashSet.empty HashSet.empty)
  where
    go :: Guard -> Visited -> HashSet Position -> [Position]
    go guard@(position, _) visited acc =
      let guard' = moveGuard guard
          acc' = HashSet.insert position acc
          visited' = HashSet.insert guard visited
       in -- If we have hit a loop or if the guard can't move anymore, finish prediction
          if HashSet.member guard visited || guard == guard'
            then HashSet.toList acc'
            else go guard' visited' acc'

    moveGuard :: Guard -> Guard
    moveGuard guard =
      let guard'@(position', _) = moveForward guard
          mObstacle = M.lookup position' labMap
       in case mObstacle of
            Nothing -> guard
            Just '#' -> turnRight guard
            Just _ -> guard'

    moveForward :: Guard -> Guard
    moveForward ((row, col), '^') = ((row - 1, col), '^')
    moveForward ((row, col), 'v') = ((row + 1, col), 'v')
    moveForward ((row, col), '>') = ((row, col + 1), '>')
    moveForward ((row, col), '<') = ((row, col - 1), '<')
    moveForward guard = guard

    turnRight :: Guard -> Guard
    turnRight (pos, '^') = (pos, '>')
    turnRight (pos, '>') = (pos, 'v')
    turnRight (pos, 'v') = (pos, '<')
    turnRight (pos, '<') = (pos, '^')
    turnRight guard = guard
