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
day7Part2 fp = do
  contents <- readFile fp
  return $ syncCrabs1 $ parseInput contents

parseInput :: String -> [Int]
parseInput str = map read $ splitOn "," str

syncCrabs :: [Int] -> Int
syncCrabs xs = case median xs of
  Left x -> sum $ map (\y -> abs (y - x)) xs
  Right (x1, x2) -> min (sum $ map (\y -> abs (y - x1)) xs) (sum $ map (\y -> abs (y - x2)) xs)

-- | Search solution between mean and median
syncCrabs1 :: [Int] -> Int
syncCrabs1 xs = case median xs of
  Left xMed -> minimum $ map (syncCrabs1Sol xs) $ if fromIntegral xMed < xMean then [xMed .. ceiling xMean] else [floor xMean .. xMed]
  Right (xMed1, xMed2) -> minimum $ map (syncCrabs1Sol xs) $ if fromIntegral xMed2 < xMean then [xMed1 .. ceiling xMean] else [floor xMean .. xMed2]
  where
    xMean = mean xs

syncCrabs1Sol :: [Int] -> Int -> Int
syncCrabs1Sol xs x = sum $ map (\y -> mysum $ abs (y - x)) xs
  where
    mysum n = n * (n + 1) `div` 2 -- sum from 1 to n

median :: [Int] -> Either Int (Int, Int)
median x =
  if odd n
    then Left $ xsort !! (n `div` 2)
    else Right (floor mid, ceiling mid)
  where
    n = length x
    xsort = sort x
    mid = (fromIntegral (xsort !! (n `div` 2 - 1)) + fromIntegral (xsort !! (n `div` 2))) / 2 :: Float

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
