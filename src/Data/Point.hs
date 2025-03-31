module Data.Point
  ( Point,
    substract,
    add,
    distance,
  )
where

type Point = (Int, Int)

substract :: Point -> (Int, Int) -> (Int, Int)
substract (x, y) (dx, dy) = (x - dx, y - dy)

add :: Point -> (Int, Int) -> (Int, Int)
add (x, y) (dx, dy) = (x + dx, y + dy)

distance :: Point -> Point -> (Int, Int)
distance (xrow, xcol) (yrow, ycol) = (xrow - yrow, xcol - ycol)
