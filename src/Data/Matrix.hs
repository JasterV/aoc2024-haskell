{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix
  ( Matrix,
    buildMatrix,
    lookupValue,
    lookup,
    lookupMultiple,
    filterWithKey,
    size,
    insert,
    groupByWith,
    filter,
    isInBounds,
  )
where

import Data.List (find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Point (Point)
import Prelude hiding (filter, lookup)

newtype Matrix v = Matrix (Map (Int, Int) v)

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

isInBounds :: Point -> Matrix v -> Bool
isInBounds pos (Matrix hmap) = Map.member pos hmap

filterWithKey :: (Point -> v -> Bool) -> Matrix v -> Matrix v
filterWithKey f (Matrix hmap) = Matrix (Map.filterWithKey f hmap)

filter :: (v -> Bool) -> Matrix v -> Matrix v
filter f (Matrix hmap) = Matrix (Map.filter f hmap)

lookup :: Point -> Matrix v -> Maybe v
lookup position (Matrix hmap) = Map.lookup position hmap

lookupMultiple :: [Point] -> Matrix v -> [v]
lookupMultiple positions matrix = mapMaybe (`lookup` matrix) positions

insert :: Point -> v -> Matrix v -> Matrix v
insert position value (Matrix hmap) =
  Matrix $
    if Map.member position hmap
      then Map.insert position value hmap
      else hmap

{--
  Search for the given value on the matrix.
  Return the position of the first match if found and nothing if it doens't exist.
--}
lookupValue :: (Eq v) => v -> Matrix v -> Maybe Point
lookupValue v (Matrix hmap) =
  let entries = Map.toAscList hmap
      mEntry = find ((== v) . snd) entries
   in fst <$> mEntry

{--
Group elements given a function.
The function receives an entry of the matrix and returns a pair of key -> value.
The values are grouped in order.
--}
groupByWith :: forall v a b. (Ord a) => ((Point, v) -> (a, b)) -> Matrix v -> Map a [b]
groupByWith f (Matrix hmap) =
  let sortedEntries = Map.toAscList hmap
   in foldr (insert' . f) Map.empty sortedEntries
  where
    insert' :: (a, b) -> Map a [b] -> Map a [b]
    insert' (key, value) =
      Map.alter
        ( \case
            Nothing -> Just [value]
            Just xs -> Just (value : xs)
        )
        key
