module Day11 (partOne, partTwo, nextStone) where

type Stone = Int

partOne :: String -> Int
partOne raw = sum $ map (blinkN 25) $ parseStones raw

partTwo :: String -> Int
partTwo raw = sum $ map (blinkN 75) $ parseStones raw

blinkN :: Int -> Stone -> Int
blinkN 0 _ = 1
blinkN blinks stone = sum $ map (blinkN (blinks - 1)) $ nextStone stone

-- blinkN blinks stone = concatMap (blinkN (blinks - 1)) (nextStone stone)

nextStone :: Stone -> [Stone]
nextStone stone
  | stone == 0 = [1]
  | even lengthDigits =
      [ read $ take (lengthDigits `div` 2) digits,
        read $ drop (lengthDigits `div` 2) digits
      ]
  | otherwise = [stone * 2024]
  where
    digits :: [Char]
    digits = show stone

    lengthDigits :: Int
    lengthDigits = length digits

parseStones :: String -> [Stone]
parseStones = map read . words
