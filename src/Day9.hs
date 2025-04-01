module Day9 (partOne, partTwo) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe

type FileId = Int

type Space = Int

type Position = Int

data Block = FileBlock Position FileId | FreeBlock Position
  deriving (Show, Eq)

data Object = File Position Space FileId | FreeSpace Position Space
  deriving (Show, Eq)

partOne :: String -> Int
partOne input = calculateCheckSum compactFiles
  where
    blocks = concatMap getBlocks $ parseDisk input

    compactFiles :: [Block]
    compactFiles = go blocks (reverse blocks) []
      where
        go [] _ acc = reverse acc
        go _ [] acc = reverse acc
        go (lBlock : xs) (rBlock : ys) acc
          | lIndex > rIndex = reverse acc
          | otherwise = case (lBlock, rBlock) of
              (FileBlock _ _, _) -> go xs (rBlock : ys) (lBlock : acc)
              (FreeBlock lPos, FileBlock _ fId) -> go xs ys (FileBlock lPos fId : acc)
              (FreeBlock _, FreeBlock _) -> go (lBlock : xs) ys acc
          where
            lIndex = getBlockPosition lBlock
            rIndex = getBlockPosition rBlock

partTwo :: String -> Int
partTwo input = calculateCheckSum $ concatMap getBlocks compactFiles
  where
    objects = parseDisk input

    compactFiles :: [Object]
    compactFiles = go (reverse objects)
      where
        go [] = []
        go ((FreeSpace _ _) : ys) = go ys
        go (file@(File pos space fId) : ys) =
          let mYs = replaceLeftMostSpace (reverse ys) (pos, space, fId)
           in case mYs of
                Nothing -> go ys ++ [file]
                Just ys' -> go $ reverse ys'

        replaceLeftMostSpace :: [Object] -> (Position, Space, FileId) -> Maybe [Object]
        replaceLeftMostSpace [] _ = Nothing
        replaceLeftMostSpace (left : xs) right@(_, rSpace, fId) =
          case left of
            (File {}) -> (left :) <$> replaceLeftMostSpace xs right
            (FreeSpace lPos lSpace)
              | lSpace == rSpace -> Just (File lPos rSpace fId : xs)
              | lSpace > rSpace -> Just (File lPos rSpace fId : FreeSpace (lPos + rSpace) (lSpace - rSpace) : xs)
              | otherwise -> (left :) <$> replaceLeftMostSpace xs right

-- Blocks & Objects

calculateCheckSum :: [Block] -> Int
calculateCheckSum blocks = sum $ mapMaybe blockCheckSum blocks

getBlocks :: Object -> [Block]
getBlocks (File position space fileId) = map (`FileBlock` fileId) [position .. (position + space - 1)]
getBlocks (FreeSpace position space) = map FreeBlock [position .. (position + space - 1)]

getBlockPosition :: Block -> Position
getBlockPosition (FileBlock pos _) = pos
getBlockPosition (FreeBlock pos) = pos

blockCheckSum :: Block -> Maybe Int
blockCheckSum (FileBlock pos fileId) = Just (pos * fileId)
blockCheckSum _ = Nothing

-- Parsing

data ParseStep = ParseFile | ParseFree

parseDisk :: String -> [Object]
parseDisk input = go (parseInts input) ParseFile 0 0 []
  where
    go :: [Int] -> ParseStep -> FileId -> Position -> [Object] -> [Object]
    go [] _ _ _ acc = reverse acc
    go (space : xs) _ fileId position [] = go xs ParseFree (fileId + 1) (position + space) [File position space fileId]
    go (space : xs) ParseFree fileId position acc = go xs ParseFile fileId (position + space) (FreeSpace position space : acc)
    go (space : xs) ParseFile fileId position acc = go xs ParseFree (fileId + 1) (position + space) (File position space fileId : acc)

    parseInts :: String -> [Int]
    parseInts = map digitToInt . filter isDigit
