module Day7
  ( day7Part1,
    day7Part2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)

day7Part1 :: FilePath -> IO Int
day7Part1 fp = do
  contents <- readFile fp
  return $ syncCrabs $ parseInput contents

day7Part2 :: FilePath -> IO Int
day7Part2 _ = return (-1)

parseInput :: String -> [Int]
parseInput str = map read $ splitOn "," str

syncCrabs :: [Int] -> Int
syncCrabs xs = case median xs of
  Left x -> sum $ map (\y -> abs (y - x)) xs
  Right (x1, x2) -> min (sum $ map (\y -> abs (y - x1)) xs) (sum $ map (\y -> abs (y - x2)) xs)

median :: [Int] -> Either Int (Int, Int)
median x =
  if odd n
    then Left $ xsort !! (n `div` 2)
    else Right (xsort !! (n `div` 2 - 1), xsort !! (n `div` 2))
  where
    n = length x
    xsort = sort x
