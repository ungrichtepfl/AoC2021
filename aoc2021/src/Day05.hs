module Day05
  ( day05Part1,
    day05Part2,
  )
where

import Data.List.Split (splitOn)

--import qualified Debug.Trace as Dt

day05Part1 :: FilePath -> IO Int
day05Part1 fp = do
  contents <- readFile fp
  let ls = parseLines $ lines contents :: [Line]
  let finalFloor = markLines1 initFloor ls
  --  putStrLn $ "Final floor:\n" ++ show finalFloor
  return $ overlaps 2 finalFloor

day05Part2 :: FilePath -> IO Int
day05Part2 fp = do
  contents <- readFile fp
  let ls = parseLines $ lines contents :: [Line]
  let finalFloor = markLines2 initFloor ls
  --  putStrLn $ "Final floor:\n" ++ show finalFloor
  return $ overlaps 2 finalFloor

data OceanPoint = OceanPoint
  { position :: (Int, Int),
    vents :: Int
  }
  deriving (Show)

newOceanPoint :: (Int, Int) -> OceanPoint
newOceanPoint pos = OceanPoint pos 1

newtype OceanFloor = OceanFloor [OceanPoint] deriving (Show)

data Line = Line
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show)

initFloor :: OceanFloor
initFloor = OceanFloor []

markLines1 :: OceanFloor -> [Line] -> OceanFloor
markLines1 = foldr markLine1

markLines2 :: OceanFloor -> [Line] -> OceanFloor
markLines2 = foldr markLine2

markLine1 :: Line -> OceanFloor -> OceanFloor
markLine1 l oceanFloor = if null conns then oceanFloor else foldr addToOceanFloor oceanFloor conns
  where
    conns = connection1 l

markLine2 :: Line -> OceanFloor -> OceanFloor
markLine2 l oceanFloor = if null conns then oceanFloor else foldr addToOceanFloor oceanFloor conns
  where
    conns = connection2 l

addToOceanFloor :: (Int, Int) -> OceanFloor -> OceanFloor
addToOceanFloor pos (OceanFloor []) = OceanFloor [newOceanPoint pos]
addToOceanFloor pos (OceanFloor ops) =
  if any (posContained pos) ops
    then OceanFloor $ addCrossing ops pos
    else OceanFloor $ newOceanPoint pos : ops

addCrossing :: [OceanPoint] -> (Int, Int) -> [OceanPoint]
addCrossing [] _ = []
addCrossing (fop@(OceanPoint pos1 n) : ops') pos2
  | pos1 == pos2 = OceanPoint pos1 (n + 1) : addCrossing ops' pos2
  | otherwise = fop : addCrossing ops' pos2

posContained :: (Int, Int) -> OceanPoint -> Bool
posContained p2 (OceanPoint p1 _) = p1 == p2

-- | without diagonals
connection1 :: Line -> [(Int, Int)]
connection1 (Line (x1, y1) (x2, y2))
  | x1 == x2 = zip (replicate (abs (y1 - y2) + 1) x1) (if y2 > y1 then [y1 .. y2] else [y2 .. y1])
  | y1 == y2 = zip (if x2 > x1 then [x1 .. x2] else [x2 .. x1]) (replicate (abs (x1 - x2) + 1) y1)
  | otherwise = []

-- | with diagonals
connection2 :: Line -> [(Int, Int)]
connection2 (Line (x1, y1) (x2, y2))
  | x1 == x2 = zip (replicate (abs (y1 - y2) + 1) x1) (if y2 > y1 then [y1 .. y2] else [y2 .. y1])
  | y1 == y2 = zip (if x2 > x1 then [x1 .. x2] else [x2 .. x1]) (replicate (abs (x1 - x2) + 1) y1)
  | abs (y2 - y1) == abs (x2 - x1) =
    zip (if x2 >= x1 then [x1 .. x2] else [x1, (x1 -1) .. x2]) (if y2 >= y1 then [y1 .. y2] else [y1, (y1 -1) .. y2])
  | otherwise = []

parseLines :: [String] -> [Line]
parseLines = map str2line
  where
    str2line :: String -> Line
    str2line str =
      let [[x1, y1], [x2, y2]] = map (map read . splitOn ",") (splitOn " -> " str) :: [[Int]]
       in Line (x1, y1) (x2, y2) -- x is column and y is row

overlaps :: Int -> OceanFloor -> Int
overlaps n (OceanFloor ops) = length $ filter (\(OceanPoint _ n') -> n' >= n) ops
