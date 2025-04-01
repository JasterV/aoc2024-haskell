module Day9 (partOne, partTwo) where

import Data.Char (digitToInt, isDigit)

type FileId = Int

data Block = File FileId | Free
  deriving (Show, Eq)

partOne :: String -> Int
partOne input = sum $ zipWith (*) [0 ..] $ compactFiles $ parseDisk input

partTwo :: String -> Int
partTwo _ = 0

-- Compacting
compactFiles :: [Block] -> [FileId]
compactFiles blocks = go enumeratedBlocks (reverse enumeratedBlocks) []
  where
    enumeratedBlocks :: [(Int, Block)]
    enumeratedBlocks = zip [0 ..] blocks

    go [] _ acc = reverse acc
    go _ [] acc = reverse acc
    go ((lIndex, lBlock) : xs) ((rIndex, rBlock) : ys) acc
      | lIndex > rIndex = reverse acc
      | otherwise = case (lBlock, rBlock) of
          (File fileId, _) -> go xs ((rIndex, rBlock) : ys) (fileId : acc)
          (Free, File fileId) -> go xs ys (fileId : acc)
          (Free, Free) -> go ((lIndex, lBlock) : xs) ys acc

-- Parsing

data ParseStep = ParseFile | ParseFree

parseDisk :: String -> [Block]
parseDisk input = go (parseInts input) ParseFile 0 []
  where
    go [] _ _id acc = reverse acc
    go (space : xs) _ fileId [] = go xs ParseFree (fileId + 1) (replicate space (File fileId))
    go (space : xs) ParseFree fileId acc = go xs ParseFile fileId (replicate space Free ++ acc)
    go (space : xs) ParseFile fileId acc = go xs ParseFree (fileId + 1) (replicate space (File fileId) ++ acc)

    parseInts :: String -> [Int]
    parseInts = map digitToInt . filter isDigit
