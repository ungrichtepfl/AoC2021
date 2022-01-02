module Day6
  ( day6Part1,
    day6Part2,
  )
where

import Data.List.Split (splitOn)

day6Part1 :: FilePath -> IO Int
day6Part1 fp = do
  contents <- readFile fp
  let fishList = getFishList contents
  return $ fishAfterN 80 fishList

day6Part2 :: FilePath -> IO Int
day6Part2 fp = do
  contents <- readFile fp
  let fishList = getFishList contents
  return $ fishAfterNFPD 256 $ initFPD fishList

getFishList :: String -> [Int]
getFishList str = map read $ splitOn "," str

-- Simple version:
update :: [Int] -> [Int]
update [] = []
update (x : xs)
  | x == 0 = 8 : 6 : update xs
  | otherwise = x -1 : update xs

updateN :: Int -> [Int] -> [Int]
updateN n = foldr (.) id $ replicate n update

fishAfterN :: Int -> [Int] -> Int
fishAfterN n ip = length $ updateN n ip

-- Efficient version:

-- | First int nb of fish 0 days left until spawn, Second int nb fish 1 day left until span, etc
type FishPerDay = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

fishAfterNFPD :: Int -> FishPerDay -> Int
fishAfterNFPD n = sumFPD . updateNFPD n

updateNFPD :: Int -> FishPerDay -> FishPerDay
updateNFPD n = foldr (.) id $ replicate n updateFPD

sumFPD :: FishPerDay -> Int
sumFPD (f0, f1, f2, f3, f4, f5, f6, f7, f8) = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8

updateFPD :: FishPerDay -> FishPerDay
updateFPD (f0, f1, f2, f3, f4, f5, f6, f7, f8) = (f1, f2, f3, f4, f5, f6, f7 + f0, f8, f0)

initFPD :: [Int] -> FishPerDay
initFPD xs = fillFPD xs (0, 0, 0, 0, 0, 0, 0, 0, 0)

fillFPD :: [Int] -> FishPerDay -> FishPerDay
fillFPD [] fpd = fpd
fillFPD (x : xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8)
  | x == 0 = fillFPD xs (f0 + 1, f1, f2, f3, f4, f5, f6, f7, f8)
  | x == 1 = fillFPD xs (f0, f1 + 1, f2, f3, f4, f5, f6, f7, f8)
  | x == 2 = fillFPD xs (f0, f1, f2 + 1, f3, f4, f5, f6, f7, f8)
  | x == 3 = fillFPD xs (f0, f1, f2, f3 + 1, f4, f5, f6, f7, f8)
  | x == 4 = fillFPD xs (f0, f1, f2, f3, f4 + 1, f5, f6, f7, f8)
  | x == 5 = fillFPD xs (f0, f1, f2, f3, f4, f5 + 1, f6, f7, f8)
  | x == 6 = fillFPD xs (f0, f1, f2, f3, f4, f5, f6 + 1, f7, f8)
  | x == 7 = fillFPD xs (f0, f1, f2, f3, f4, f5, f6, f7 + 1, f8)
  | x == 8 = fillFPD xs (f0, f1, f2, f3, f4, f5, f6, f7 + 1, f8 + 1)
  | otherwise = error "Input has to be between 0 or 7."
