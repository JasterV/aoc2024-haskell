module Day6 (partOne, partTwo) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Matrix (Matrix, Position)
import qualified Data.Matrix as M
import Day6.Guard (Guard (..))
import qualified Day6.Guard as G

data Error = GuardNotFoundError
  deriving (Eq, Show)

partOne :: String -> Either Error Int
partOne input = do
  let labMap = parseLabMap input
  guard <- findGuard labMap
  let route = predictGuardsRoute guard labMap
  return (length route)

partTwo :: String -> Either Error Int
partTwo input = do
  let labMap = parseLabMap input
  guard <- findGuard labMap
  let initialPosition = position guard
      route = predictGuardsRoute guard labMap
      candidates = filter (/= initialPosition) route

  return $
    length $
      filter (hasLoop guard) $
        map (\pos -> M.insert pos '#' labMap) candidates

-- Laboratory Map
type LabMap = Matrix Char

type Visited = HashSet Guard

parseLabMap :: String -> LabMap
parseLabMap = M.buildMatrix . lines

findGuard :: LabMap -> Either Error Guard
findGuard matrix =
  let mUp = M.lookupValue '^' matrix
      mDown = M.lookupValue 'v' matrix
      mRight = M.lookupValue '>' matrix
      mLeft = M.lookupValue '<' matrix
   in case [mUp, mDown, mRight, mLeft] of
        [Just pos, _, _, _] -> Right (Guard pos G.Up)
        [_, Just pos, _, _] -> Right (Guard pos G.Down)
        [_, _, Just pos, _] -> Right (Guard pos G.Right)
        [_, _, _, Just pos] -> Right (Guard pos G.Left)
        _ -> Left GuardNotFoundError

hasLoop :: Guard -> LabMap -> Bool
hasLoop initialGuard labMap = go initialGuard HashSet.empty
  where
    go :: Guard -> Visited -> Bool
    go guard visited =
      let guard' = moveGuard guard labMap
          visited' = HashSet.insert guard visited
       in HashSet.member guard visited || ((guard /= guard') && go guard' visited')

predictGuardsRoute :: Guard -> LabMap -> [Position]
predictGuardsRoute initialGuard labMap = go initialGuard HashSet.empty HashSet.empty
  where
    go :: Guard -> Visited -> HashSet Position -> [Position]
    go guard visited acc =
      let guard' = moveGuard guard labMap
          acc' = HashSet.insert (position guard) acc
          visited' = HashSet.insert guard visited
       in -- If we have hit a loop or if the guard can't move anymore, finish prediction
          if HashSet.member guard visited || guard == guard'
            then HashSet.toList acc'
            else go guard' visited' acc'

moveGuard :: Guard -> LabMap -> Guard
moveGuard guard labMap =
  let guard' = G.moveForward guard
      mObstacle = M.lookup (position guard') labMap
   in case mObstacle of
        Nothing -> guard
        Just '#' -> G.turnRight guard
        Just _ -> guard'
