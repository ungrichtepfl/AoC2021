module Day08
  ( day08Part1,
    day08Part2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

day08Part1 :: FilePath -> IO Int
day08Part1 fp = do
  contents <- readFile fp
  let sigs = parseInput contents
  return $ countLength [2, 3, 4, 7] sigs

day08Part2 :: FilePath -> IO Int
day08Part2 fp = do
  contents <- readFile fp
  let sigs = parseInput contents
  let nums = deduceAll sigs
  putStrLn $ "All numbers are:\n" ++ show nums
  return $ sum nums

type Patterns = [String]

type Digits = [String]

newtype Signal = Signal (Patterns, Digits) deriving (Show)

type SignalLength = Int

type WireToSeg = Map.Map Char Char

type WireToPossSeg = Map.Map Char String

wtpsToWts :: WireToPossSeg -> WireToSeg
wtpsToWts = Map.map check
  where
    check p = if length p == 1 then head p else error "No solution found!"

initWireToPossSeg :: WireToPossSeg
initWireToPossSeg =
  Map.fromList
    [ ('a', "abcdefg"),
      ('b', "abcdefg"),
      ('c', "abcdefg"),
      ('d', "abcdefg"),
      ('e', "abcdefg"),
      ('f', "abcdefg"),
      ('g', "abcdefg")
    ]

realDigits :: Map.Map String Int
realDigits =
  Map.fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

deduceAll :: [Signal] -> [Int]
deduceAll = map signalToInt

signalToInt :: Signal -> Int
signalToInt sig@(Signal (_, digits)) = digitsToInt (deduce sig) digits

digitsToInt :: WireToSeg -> Digits -> Int
digitsToInt wts digits = fst $ foldr (\dig (out, n) -> (digitToInt dig wts * 10 ^ n + out, n + 1)) ((0, 0) :: (Int, Int)) digits

digitToInt :: String -> WireToSeg -> Int
digitToInt str wts = case Map.lookup (sort $ transform str wts) realDigits of
  Just x -> x
  _ -> error "No key found."

transform :: String -> WireToSeg -> String
transform [] _ = []
transform (c : cs) wts = case Map.lookup c wts of
  Just c' -> c' : transform cs wts
  _ -> error "No key found."

deduce :: Signal -> WireToSeg
deduce (Signal (pats, _)) = filter6 (filter (\x -> length x == 6) pats) $ filter234 (filter (\x -> 3 <= length x && length x <= 4) pats) initWireToPossSeg

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

filter6 :: Patterns -> WireToPossSeg -> WireToSeg
filter6 pats wtps = wtpsToWts $ foldr filterOut wtps pats
  where
    filterOut :: String -> WireToPossSeg -> WireToPossSeg
    filterOut pat wtps1 = case Map.lookup (findMissing1 pat) wtps1 of
      Just "cf" -> Map.mapWithKey (\k l -> if k == findMissing1 pat then "c" else if l == "cf" then "f" else l) wtps1
      Just "eg" -> Map.mapWithKey (\k l -> if k == findMissing1 pat then "e" else if l == "eg" then "g" else l) wtps1
      Just "bd" -> Map.mapWithKey (\k l -> if k == findMissing1 pat then "d" else if l == "bd" then "b" else l) wtps1
      _ -> error "Can only handle input that is already filtered by filter234 and patterns of length 6."

findMissing1 :: String -> Char
findMissing1 str = head $ filter (`notElem` str) "abcdefg"

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
