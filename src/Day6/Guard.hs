{-# LANGUAGE DeriveGeneric #-}

module Day6.Guard (Guard (..), Direction (..), moveForward, turnRight) where

import Data.Hashable
import Data.Matrix (Position)
import GHC.Generics (Generic)
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right
  deriving (Eq, Generic)

instance Hashable Direction

data Guard = Guard {position :: Position, directon :: Direction}
  deriving (Eq, Generic)

instance Hashable Guard

moveForward :: Guard -> Guard
moveForward (Guard (row, col) Up) = Guard (row - 1, col) Up
moveForward (Guard (row, col) Down) = Guard (row + 1, col) Down
moveForward (Guard (row, col) Right) = Guard (row, col + 1) Right
moveForward (Guard (row, col) Left) = Guard (row, col - 1) Left

turnRight :: Guard -> Guard
turnRight (Guard pos Up) = Guard pos Right
turnRight (Guard pos Right) = Guard pos Down
turnRight (Guard pos Down) = Guard pos Left
turnRight (Guard pos Left) = Guard pos Up
