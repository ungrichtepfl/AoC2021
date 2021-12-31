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
  return $ fishAfterN 256 fishList

update :: [Int] -> [Int]
update [] = []
update (x : xs)
  | x == 0 = 8 : 6 : update xs
  | otherwise = x -1 : update xs

updateN :: Int -> [Int] -> [Int]
updateN n ip = iterate update ip !! n

fishAfterN :: Int -> [Int] -> Int
fishAfterN n ip = length $ updateN n ip

getFishList :: String -> [Int]
getFishList str = map read $ splitOn "," str
