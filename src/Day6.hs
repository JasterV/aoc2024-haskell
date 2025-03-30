module Day6 (partOne, partTwo) where

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

data FoldLabMapResult a = Success a | InfiniteLoopError

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

foldLabMapWithGuard :: a -> (Guard -> a -> a) -> Guard -> LabMap -> FoldLabMapResult a
foldLabMapWithGuard initialValue f initialGuard labMap = go initialGuard HashSet.empty initialValue
  where
    go guard visited acc =
      let guard' = moveGuard guard
          acc' = f guard acc
          visited' = HashSet.insert guard visited
       in if HashSet.member guard visited
            then InfiniteLoopError
            else
              if guard == guard'
                then Success acc'
                else go guard' visited' acc'

    moveGuard :: Guard -> Guard
    moveGuard guard =
      let guard' = G.moveForward guard
          mObstacle = M.lookup (position guard') labMap
       in case mObstacle of
            Nothing -> guard
            Just '#' -> G.turnRight guard
            Just _ -> guard'

hasLoop :: Guard -> LabMap -> Bool
hasLoop initialGuard labMap = case foldLabMapWithGuard [] (\_ _ -> []) initialGuard labMap of
  InfiniteLoopError -> True
  _ -> False

predictGuardsRoute :: Guard -> LabMap -> [Position]
predictGuardsRoute guard labMap = case foldLabMapWithGuard HashSet.empty (HashSet.insert . position) guard labMap of
  InfiniteLoopError -> []
  Success set -> HashSet.toList set
