module Day8
  ( day8Part1,
    day8Part2,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

day8Part1 :: FilePath -> IO Int
day8Part1 fp = do
  contents <- readFile fp
  let sigs = parseInput contents
  return $ countLength [2, 3, 4, 7] sigs

day8Part2 :: FilePath -> IO Int
day8Part2 fp = return (-1)

type Patterns = [String]

type Digits = [String]

newtype Signal = Signal (Patterns, Digits) deriving (Show)

type SignalLength = Int

type WireToSeg = Map.Map Char Char

type WireToPossSeg = Map.Map Char String

initWireToPossSeg :: WireToPossSeg
initWireToPossSeg =
  Map.fromList
    [ ('a', "abcdefg"),
      ('b', "abcdefg"),
      ('a', "abcdefg"),
      ('d', "abcdefg"),
      ('e', "abcdefg"),
      ('f', "abcdefg"),
      ('g', "abcdefg")
    ]

filter234 :: Patterns -> WireToPossSeg -> WireToPossSeg
filter234 pats wtps = foldr filterOut wtps pats
  where
    filterOut :: String -> WireToPossSeg -> WireToPossSeg
    filterOut pat wtps' =
      let toFilter = case length pat of
            2 -> "cf"
            3 -> "acf"
            4 -> "bcdf"
            _ -> error "Error exceps only patterns of length 2, 3 or 4 (correspond to 1, 7 and 4)."
       in Map.mapWithKey (\k p -> if k `elem` pat then filter (`elem` toFilter) p else filter (`notElem` toFilter) p) wtps'

countLength :: [SignalLength] -> [Signal] -> Int
countLength _ [] = 0
countLength ls (Signal (_, dig) : sgns) = length (filter (`elem` ls) (digitLengts dig)) + countLength ls sgns

digitLengts :: Digits -> [Int]
digitLengts = map length

parseInput :: String -> [Signal]
parseInput ip = map parseLine (lines ip)

parseLine :: String -> Signal
parseLine l =
  let [pat, digit] = (map words . splitOn " | ") l
   in Signal (pat, digit)
