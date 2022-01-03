module Day8
  ( day8Part1,
    day8Part2,
  )
where

import Data.List.Split (splitOn)

day8Part1 :: FilePath -> IO Int
day8Part1 fp = do
  contents <- readFile fp
  let sigs = parseInput contents
  return $ countLength [2, 3, 4, 7] sigs

day8Part2 :: FilePath -> IO Int
day8Part2 fp = return (-1)

type Patterns = [String]

type Digits = [String]

newtype Signal = Signal ([Patterns], [Digits]) deriving (Show)

type SignalLength = Int

countLength :: [SignalLength] -> Signal -> Int
countLength _ (Signal (_, [])) = 0
countLength ls (Signal (ps, d : ds)) = length (filter (`elem` ls) (digitLengts d)) + countLength ls (Signal (ps, ds))

digitLengts :: Digits -> [Int]
digitLengts = map length

parseInput :: String -> Signal
parseInput ip = Signal $ parseLines (lines ip)

parseLines :: [String] -> ([Patterns], [Digits])
parseLines [] = ([], [])
parseLines (l : lns) =
  let (pat, digit) = parseLine l
      (patNex, digitNex) = parseLines lns
   in (pat : patNex, digit : digitNex)

parseLine :: String -> (Patterns, Digits)
parseLine l =
  let [pat, digit] = (map words . splitOn " | ") l
   in (pat, digit)
