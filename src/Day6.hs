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
partOne input = length <$> predictGuardsRoute (parseLabMap input)

partTwo :: String -> Either Error Int
partTwo _input = Right 0

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

predictGuardsRoute :: LabMap -> Either Error [Position]
predictGuardsRoute labMap = do
  guard <- findGuard labMap
  return $ go guard HashSet.empty HashSet.empty
  where
    go :: Guard -> Visited -> HashSet Position -> [Position]
    go guard visited acc =
      let guard' = moveGuard guard
          acc' = HashSet.insert (position guard) acc
          visited' = HashSet.insert guard visited
       in -- If we have hit a loop or if the guard can't move anymore, finish prediction
          if HashSet.member guard visited || (guard == guard')
            then HashSet.toList acc'
            else go guard' visited' acc'

    moveGuard :: Guard -> Guard
    moveGuard guard =
      let guard' = G.moveForward guard
          mObstacle = M.lookup (position guard') labMap
       in case mObstacle of
            Nothing -> guard
            Just '#' -> G.turnRight guard
            Just _ -> guard'
