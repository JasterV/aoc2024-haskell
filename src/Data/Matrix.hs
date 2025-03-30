{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix
  ( Matrix,
    Position,
    buildMatrix,
    groupWith,
    lookupValue,
    lookup,
    lookupMultiple,
    filterWithKey,
    size,
    insert,
  )
where

import qualified Data.IntMap.Lazy as IntMap
import Data.List (find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Prelude hiding (lookup)

newtype Matrix v = Matrix (Map (Int, Int) v)

type Position = (Int, Int)

buildMatrix :: [[a]] -> Matrix a
buildMatrix xs = Matrix (go xs 0 Map.empty)
  where
    go [] _ acc = acc
    go (x : xs') row acc =
      let acc' = parseRow x (row, 0) acc
       in go xs' (row + 1) acc'

    parseRow [] _ acc = acc
    parseRow (x : xs') (row, column) acc =
      let acc' = Map.insert (row, column) x acc
       in parseRow xs' (row, column + 1) acc'

size :: Matrix v -> Int
size (Matrix hmap) = Map.size hmap

filterWithKey :: (Position -> v -> Bool) -> Matrix v -> Matrix v
filterWithKey f (Matrix hmap) = Matrix (Map.filterWithKey f hmap)

lookup :: Position -> Matrix v -> Maybe v
lookup position (Matrix hmap) = Map.lookup position hmap

lookupMultiple :: [Position] -> Matrix v -> [v]
lookupMultiple positions matrix = mapMaybe (`lookup` matrix) positions

insert :: Position -> v -> Matrix v -> Matrix v
insert position value (Matrix hmap) =
  Matrix $
    if Map.member position hmap
      then Map.insert position value hmap
      else hmap

{--
  Search for the given value on the matrix.
  Return the position of the first match if found and nothing if it doens't exist.
--}
lookupValue :: (Eq v) => v -> Matrix v -> Maybe Position
lookupValue v (Matrix hmap) =
  let entries = Map.toAscList hmap
      mEntry = find ((== v) . snd) entries
   in fst <$> mEntry

{--
Given a matrix of elements and a function mapping a position into an aggregation of its values,
group the elements by the aggregation result.
The values are grouped in order.
--}
groupWith :: forall v. (Position -> Int) -> Matrix v -> [[v]]
groupWith f (Matrix hmap) =
  let sortedEntries = Map.toAscList hmap
      intMap = foldr (\(position, value) -> insertValue (f position) value) IntMap.empty sortedEntries
   in IntMap.elems intMap
  where
    insertValue key value =
      IntMap.alter
        ( \case
            Nothing -> Just [value]
            Just xs -> Just (value : xs)
        )
        key
