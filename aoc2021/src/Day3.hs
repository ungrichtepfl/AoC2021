module Day3
  ( day3Part1,
    day3Part2,
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

day3Part2 :: FilePath -> IO Int
day3Part2 fp = do
  contents <- readFile fp
  let contentLines = lines contents
  let nbLines = length contentLines
  let intList = transformToInt contentLines
  let mostCommonNumber = getMostCommonNumberList intList $ fromIntegral nbLines / 2
  let leastCommonNumber = getLeastCommonNumberList intList $ fromIntegral nbLines / 2
  let oxygenGeneratorRating = filterByTemplateMostCommon mostCommonNumber intList
  let co2ScrubbingRating = filterByTemplateLeastCommon leastCommonNumber intList
  putStrLn $ "Oxgen Generateor Rating: " ++ concatMap show oxygenGeneratorRating
  putStrLn $ "Co2ScrubbingRating: " ++ concatMap show co2ScrubbingRating
  return $ toDec oxygenGeneratorRating * toDec co2ScrubbingRating

filterByIdx :: Int -> [Int] -> [[Int]] -> [[Int]]
filterByIdx _ _ [] = []
filterByIdx i cmp (xs : xss)
  | cmp !! i == xs !! i = xs : filterByIdx i cmp xss
  | otherwise = filterByIdx i cmp xss

filterByTemplateMostCommon :: [Int] -> [[Int]] -> [Int]
filterByTemplateMostCommon = filterByTemplateMostCommonStartIdx 0

filterByTemplateLeastCommon :: [Int] -> [[Int]] -> [Int]
filterByTemplateLeastCommon = filterByTemplateLeastCommonStartIdx 0

filterByTemplateMostCommonStartIdx :: Int -> [Int] -> [[Int]] -> [Int]
filterByTemplateMostCommonStartIdx _ _ [xs] = xs
filterByTemplateMostCommonStartIdx i cmp xss = filterByTemplateMostCommonStartIdx (i + 1) cmpNew filteredXss
  where
    filteredXss = filterByIdx i cmp xss
    cmpNew = getMostCommonNumberList filteredXss $ fromIntegral (length filteredXss) / 2

filterByTemplateLeastCommonStartIdx :: Int -> [Int] -> [[Int]] -> [Int]
filterByTemplateLeastCommonStartIdx _ _ [xs] = xs
filterByTemplateLeastCommonStartIdx i cmp xss = filterByTemplateLeastCommonStartIdx (i + 1) cmpNew filteredXss
  where
    filteredXss = filterByIdx i cmp xss
    cmpNew = getLeastCommonNumberList filteredXss $ fromIntegral (length filteredXss) / 2

getGammaRate :: [String] -> Float -> [Int]
getGammaRate str = checkThreshHold (countOnes str)
  where
    checkThreshHold (x : xs) thresh
      | fromIntegral x > thresh = 1 : checkThreshHold xs thresh
      | otherwise = 0 : checkThreshHold xs thresh
    checkThreshHold [] _ = []

getMostCommonNumberList :: [[Int]] -> Float -> [Int]
getMostCommonNumberList input = checkThreshHold (countOnes2 input)
  where
    checkThreshHold (x : xs) thresh
      | fromIntegral x >= thresh = 1 : checkThreshHold xs thresh
      | otherwise = 0 : checkThreshHold xs thresh
    checkThreshHold [] _ = []

getLeastCommonNumberList :: [[Int]] -> Float -> [Int]
getLeastCommonNumberList input = checkThreshHold (countOnes2 input)
  where
    checkThreshHold (x : xs) thresh
      | fromIntegral x < thresh = 1 : checkThreshHold xs thresh
      | otherwise = 0 : checkThreshHold xs thresh
    checkThreshHold [] _ = []

countOnes2 :: [[Int]] -> [Int]
countOnes2 = foldr plusList []

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
