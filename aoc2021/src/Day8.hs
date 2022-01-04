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
day8Part2 fp = do 
  contents <- readFile fp
  let sigs = parseInput contents
  print $ deduce3 (head sigs)
  return (-1)

type Patterns = [String]

type Digits = [String]

newtype Signal = Signal (Patterns, Digits) deriving (Show)

type SignalLength = Int

type WireToSeg = Map.Map Char [Int]

type NumberToSeg = Map.Map Int [Int]

type NumToLength = Map.Map Int Int

initWireToSeg :: WireToSeg
initWireToSeg =
  Map.fromList
    [ ('a', [1, 1, 1, 1, 1, 1, 1]),
      ('b', [1, 1, 1, 1, 1, 1, 1]),
      ('c', [1, 1, 1, 1, 1, 1, 1]),
      ('d', [1, 1, 1, 1, 1, 1, 1]),
      ('e', [1, 1, 1, 1, 1, 1, 1]),
      ('f', [1, 1, 1, 1, 1, 1, 1]),
      ('g', [1, 1, 1, 1, 1, 1, 1])
    ]

numberToLength :: NumToLength
numberToLength =
  Map.fromList
    [ (0, 6),
      (1, 2),
      (2, 5),
      (3, 5),
      (4, 4),
      (5, 5),
      (6, 6),
      (7, 3),
      (8, 7),
      (9, 6)
    ]

numberToSeg :: NumberToSeg
numberToSeg =
  Map.fromList
    [ (0, [1, 1, 1, 0, 1, 1, 1]),
      (1, [0, 0, 1, 0, 0, 1, 0]),
      (2, [1, 0, 1, 1, 1, 0, 1]),
      (3, [1, 0, 1, 1, 0, 1, 1]),
      (4, [0, 1, 1, 1, 0, 1, 0]),
      (5, [1, 1, 0, 1, 0, 1, 1]),
      (6, [1, 1, 0, 1, 1, 1, 1]),
      (7, [1, 0, 1, 0, 0, 1, 0]),
      (8, [1, 1, 1, 1, 1, 1, 1]),
      (9, [1, 1, 1, 1, 0, 1, 1])
    ]

deduce3 :: Signal -> WireToSeg
deduce3 (Signal (pats, _)) = deduce2 pats initWireToSeg

deduce2 :: [String] -> WireToSeg -> WireToSeg
deduce2 pats wts = foldr deduce1 wts pats

deduce1 :: String -> WireToSeg -> WireToSeg
deduce1 pat wts = fst $ foldr go (wts, pat) (lookupKey (length pat) numberToLength)
  where
    -- todo make more beautiful
    go :: Int -> (WireToSeg, String) -> (WireToSeg, String)
    go x (wts', pat') = case Map.lookup x numberToSeg of
      Just toFilter -> (Map.mapWithKey (\k l -> if k `elem` pat' then land toFilter l else land (lnot toFilter) l) wts', pat')
      _ -> error "Number not in numberToSeg mapping."

lor :: [Int] -> [Int] -> [Int]
lor [] [] = []
lor (x : xs) (y : ys) = fromEnum ((abs x > 0) || (abs y > 0)) : lor xs ys
lor _ _ = error "List have to be the same length."

land :: [Int] -> [Int] -> [Int]
land [] [] = []
land (x : xs) (y : ys) = fromEnum ((abs x > 0) && (abs y > 0)) : land xs ys
land _ _ = error "List have to be the same length."

lnot :: [Int] -> [Int]
lnot [] = []
lnot (x : xs)
  | x == 0 = 1 : lnot xs
  | otherwise = 0 : lnot xs

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

lookupKey :: Eq v => v -> Map.Map k v -> [k]
lookupKey val = Map.foldrWithKey go []
  where
    go key value found =
      if value == val
        then key : found
        else found
