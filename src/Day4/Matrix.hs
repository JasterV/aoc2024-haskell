{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4.Matrix
  ( Matrix,
    buildMatrix,
    groupWith,
    lookup,
    lookupMultiple,
    filterWithKey,
    size,
  )
where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import Prelude hiding (lookup)

newtype Matrix v = Matrix (HashMap (Int, Int) v)

buildMatrix :: [[a]] -> Matrix a
buildMatrix xs = Matrix (go xs 0 HashMap.empty)
  where
    go [] _ acc = acc
    go (x : xs') row acc =
      let acc' = parseRow x (row, 0) acc
       in go xs' (row + 1) acc'

    parseRow [] _ acc = acc
    parseRow (x : xs') (row, column) acc =
      let acc' = HashMap.insert (row, column) x acc
       in parseRow xs' (row, column + 1) acc'

size :: Matrix v -> Int
size (Matrix hmap) = HashMap.size hmap

filterWithKey :: ((Int, Int) -> v -> Bool) -> Matrix v -> Matrix v
filterWithKey f (Matrix hmap) = Matrix (HashMap.filterWithKey f hmap)

lookup :: (Int, Int) -> Matrix v -> Maybe v
lookup position (Matrix hmap) = HashMap.lookup position hmap

lookupMultiple :: [(Int, Int)] -> Matrix v -> [v]
lookupMultiple positions matrix = mapMaybe (`lookup` matrix) positions

{--
Given a matrix of elements and a function mapping a position into an aggregation of its values,
group the elements by the aggregation result
--}
groupWith :: forall v. ((Int, Int) -> Int) -> Matrix v -> [[v]]
groupWith f (Matrix hmap) =
  let intMap :: IntMap [v]
      intMap = HashMap.foldrWithKey (insertValue . f) IntMap.empty hmap
   in IntMap.elems intMap
  where
    insertValue key value =
      IntMap.alter
        ( \case
            Nothing -> Just [value]
            Just xs -> Just (value : xs)
        )
        key
