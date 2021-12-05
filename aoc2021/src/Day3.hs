module Day3
  ( day3Part1,
  )
where

import Data.List (foldl')

day3Part1 :: FilePath -> IO Int
day3Part1 fp = do
  contents <- readFile fp
  let contentLines = lines contents
  let nbLines = length contentLines
  let gammaRate = getGammaRate contentLines $ fromIntegral nbLines / 2
  let epsilonRate = inverseBinary gammaRate
  putStrLn $ "Gamma Rate: " ++ concatMap show gammaRate
  putStrLn $ "Epsilon Rate: " ++ concatMap show epsilonRate
  return $ toDec gammaRate * toDec epsilonRate

getGammaRate :: [String] -> Float -> [Int]
getGammaRate str = checkThreshHold (countOnes str)
  where
    checkThreshHold (x : xs) thresh
      | fromIntegral x > thresh = 1 : checkThreshHold xs thresh
      | otherwise = 0 : checkThreshHold xs thresh
    checkThreshHold [] _ = []

countOnes :: [String] -> [Int]
countOnes str = foldr plusList [] $ transformToInt str

plusList :: [Int] -> [Int] -> [Int]
plusList (x : xs) (y : ys) = (x + y) : plusList xs ys
plusList [] ys = ys
plusList xs [] = xs

transformToInt :: [String] -> [[Int]]
transformToInt = map transformToInt2

transformToInt2 :: String -> [Int]
transformToInt2 = foldr checkChar []
  where
    checkChar xs acc
      | xs == '1' = 1 : acc
      | xs == '0' = 0 : acc
      | otherwise = error "Wrong character"

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

inverseBinary :: [Int] -> [Int]
inverseBinary [] = []
inverseBinary (x : xs)
  | x == 1 = 0 : inverseBinary xs
  | x == 0 = 1 : inverseBinary xs
  | otherwise = error "Not a binary number!"
