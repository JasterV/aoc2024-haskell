module Data.List.Extra (combinations, pairs) where

import Data.List (subsequences)
import Data.Maybe (mapMaybe)

combinations :: Int -> [a] -> [[a]]
combinations k xs = filter ((== k) . length) $ subsequences xs

pairs :: [a] -> [(a, a)]
pairs xs = mapMaybe parse $ combinations 2 xs
  where
    parse :: [a] -> Maybe (a, a)
    parse [x, y] = Just (x, y)
    parse _ = Nothing
