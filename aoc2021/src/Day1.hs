module Day1
  ( day1Part1,
    day1Part2,
  )
where

day1Part1 :: FilePath -> IO Int
day1Part1 filePath = do
  content <- readFile filePath
  return $ totalIncreases $ stringLinesToInt content

totalIncreases :: [Int] -> Int
totalIncreases (xs : ys : rest)
  | ys > xs = 1 + totalIncreases (ys : rest)
  | ys <= xs = 0 + totalIncreases (ys : rest)
totalIncreases _ = 0

day1Part2 :: FilePath -> IO Int
day1Part2 filePath = do
  content <- readFile filePath
  return $ totalIncreases . filterInts3 $ stringLinesToInt content

stringLinesToInt :: String -> [Int]
stringLinesToInt = stringToInt . lines

stringToInt :: [String] -> [Int]
stringToInt = map read

filterInts3 :: [Int] -> [Int]
filterInts3 (xs : ys : zs : rest) = (xs + ys + zs) : filterInts3 (ys : zs : rest)
filterInts3 _ = []
