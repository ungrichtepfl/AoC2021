module Day1
  ( day1Part1,
  )
where

day1Part1 :: FilePath -> IO Int
day1Part1 filePath = do
  content <- readFile filePath
  return $ totalIncreases $ stringToInt $ lines content

totalIncreases :: [Int] -> Int
totalIncreases (xs : ys : rest)
  | ys > xs = 1 + totalIncreases (ys : rest)
  | ys <= xs = 0 + totalIncreases (ys : rest)
totalIncreases _ = 0

stringToInt :: [String] -> [Int]
stringToInt = map read
