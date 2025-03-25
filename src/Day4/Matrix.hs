{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4.Matrix
  ( Matrix,
    buildMatrix,
    groupWith,
    getValue,
    getValues,
    toList,
  )
where

import Data.Foldable (find)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe

type AssocList k v = [(k, v)]

newtype Matrix v = Matrix (AssocList (Int, Int) v)

buildMatrix :: [[a]] -> Matrix a
buildMatrix xs = Matrix (go xs 0 [])
  where
    go [] _ acc = acc
    go (x : xs') row acc =
      let acc' = parseRow x (row, 0) acc
       in go xs' (row + 1) acc'

    parseRow [] _ acc = acc
    parseRow (x : xs') (row, column) acc =
      let acc' = ((row, column), x) : acc
       in parseRow xs' (row, column + 1) acc'

toList :: Matrix v -> [((Int, Int), v)]
toList (Matrix assoc) = assoc

getValue :: (Int, Int) -> Matrix v -> Maybe v
getValue position (Matrix assoc) = snd <$> find ((== position) . fst) assoc

getValues :: [(Int, Int)] -> Matrix v -> [v]
getValues positions matrix = mapMaybe (`getValue` matrix) positions

{--
Given a matrix of elements and a function mapping a position into an aggregation of its values,
group the elements by the aggregation result
--}
groupWith :: forall v. ((Int, Int) -> Int) -> Matrix v -> [[v]]
groupWith f (Matrix matrix) =
  let intMap :: IntMap [v]
      intMap = foldr (\(position, value) -> insertValue (f position) value) IntMap.empty matrix
   in IntMap.elems intMap
  where
    insertValue key value =
      IntMap.alter
        ( \case
            Nothing -> Just [value]
            Just xs -> Just (value : xs)
        )
        key
