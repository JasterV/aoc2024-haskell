module Day6 (partOne, partTwo) where

import qualified Data.HashSet as HashSet
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Point (Point)
import Day6.Guard (Guard (..))
import qualified Day6.Guard as G

data Error = GuardNotFoundError | InfiniteLoopError
  deriving (Eq, Show)

partOne :: String -> Either Error Int
partOne input = do
  let labMap = parseLabMap input
  guard <- findGuard labMap
  route <- predictGuardsRoute guard labMap
  return (length route)

partTwo :: String -> Either Error Int
partTwo input = do
  let labMap = parseLabMap input
  guard <- findGuard labMap
  route <- predictGuardsRoute guard labMap
  let initialPoint = position guard
      candidates = filter (/= initialPoint) route

  return $
    length $
      filter (hasLoop guard) $
        map (\pos -> M.insert pos '#' labMap) candidates
  where
    hasLoop initialGuard labMap = case predictGuardsRoute initialGuard labMap of
      Left InfiniteLoopError -> True
      _ -> False

-- Laboratory Map
type LabMap = Matrix Char

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

predictGuardsRoute :: Guard -> LabMap -> Either Error [Point]
predictGuardsRoute initialGuard labMap = go initialGuard HashSet.empty HashSet.empty
  where
    go guard visited acc =
      let guard' = moveGuard guard
          acc' = HashSet.insert (position guard) acc
          visited' = HashSet.insert guard visited
       in if HashSet.member guard visited
            then Left InfiniteLoopError
            else
              if guard == guard'
                then Right (HashSet.toList acc')
                else go guard' visited' acc'

    moveGuard :: Guard -> Guard
    moveGuard guard =
      let guard' = G.moveForward guard
          mObstacle = M.lookup (position guard') labMap
       in case mObstacle of
            Nothing -> guard
            Just '#' -> G.turnRight guard
            Just _ -> guard'
