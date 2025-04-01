module Day9 (partOne, partTwo) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe

type FileId = Int

type Space = Int

type Position = Int

data Block = FileBlock Position FileId | FreeBlock Position
  deriving (Show, Eq)

data Object = File Position Space FileId | FreeSpace Position Space

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
              (FreeBlock _, FileBlock _ _) -> go xs ys (rBlock : acc)
              (FreeBlock _, FreeBlock _) -> go (lBlock : xs) ys acc
          where
            lIndex = getBlockPosition lBlock
            rIndex = getBlockPosition rBlock

partTwo :: String -> Int
partTwo _ = 0

-- Blocks & Objects

calculateCheckSum :: [Block] -> Int
calculateCheckSum blocks = sum $ zipWith ((*) . fromMaybe 0 . getFileId) blocks [0 ..]

getBlocks :: Object -> [Block]
getBlocks (File position space fileId) = map (`FileBlock` fileId) [position .. (position + space - 1)]
getBlocks (FreeSpace position space) = map FreeBlock [position .. (position + space - 1)]

getFileId :: Block -> Maybe FileId
getFileId (FileBlock _ fileId) = Just fileId
getFileId _ = Nothing

getBlockPosition :: Block -> Position
getBlockPosition (FileBlock pos _) = pos
getBlockPosition (FreeBlock pos) = pos

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
