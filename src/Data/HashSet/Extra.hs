module Data.HashSet.Extra (unionMap) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

unionMap :: (Eq b) => (a -> HashSet b) -> HashSet a -> HashSet b
unionMap f set = HashSet.unions $ map f (HashSet.toList set)
