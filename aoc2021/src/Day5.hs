module Day5
  ( day5Part1,
    day5Part2,
  )
where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Matrix

--import qualified Debug.Trace as Dt

day5Part1 :: FilePath -> IO Int
day5Part1 fp = do
  contents <- readFile fp
  let ls = parseLines $ lines contents :: [Line]
  let finalFloor = markLines initFloor ls
  putStrLn $ "Final floor:\n" ++ show finalFloor
  return $ overlaps 2 finalFloor

day5Part2 :: FilePath -> IO Int
day5Part2 = undefined

data OceanPoint = OceanPoint
  { position :: (Int, Int),
    crossings :: Int
  }
  deriving (Show)

newtype OceanFloor = OceanFloor [OceanPoint] deriving (Show)

data Line = Line
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show)

initFloor :: OceanFloor
initFloor = OceanFloor []

markLines :: OceanFloor -> [Line] -> OceanFloor
markLines = foldl' markLine

zeroOceanPoint :: (Int, Int) -> OceanPoint
zeroOceanPoint pos = OceanPoint pos 0

markLine :: OceanFloor -> Line -> OceanFloor
markLine oceanFloor l = if null conns then oceanFloor else foldl' addToOceanFloor oceanFloor conns
  where conns = connection l

addToOceanFloor :: OceanFloor -> (Int, Int) -> OceanFloor
addToOceanFloor (OceanFloor []) pos = OceanFloor [zeroOceanPoint pos]
addToOceanFloor (OceanFloor ops) pos =
  if any (posContained pos) ops
    then OceanFloor $ addCrossing ops pos
    else OceanFloor $ zeroOceanPoint pos : ops
  where
    addCrossing :: [OceanPoint] -> (Int, Int) -> [OceanPoint]
    addCrossing [] _ = []
    addCrossing (fop@(OceanPoint pos1 n) : ops') pos2
      | pos1 == pos2 = OceanPoint pos1 (n + 1) : addCrossing ops' pos2
      | otherwise = fop : addCrossing ops' pos2

posContained :: (Int, Int) -> OceanPoint -> Bool
posContained p2 (OceanPoint p1 _) = p1 == p2

connection :: Line -> [(Int, Int)]
connection (Line (x1, y1) (x2, y2))
  | x1 == x2 = zip (replicate (abs (y1 - y2) + 1) x1) (if y2 > y1 then [y1 .. y2] else [y2 .. y1])
  | y1 == y2 = zip (if x2 > x1 then [x1 .. x2] else [x2 .. x1]) (replicate (abs (x1 - x2) + 1) y1)
  | otherwise = []

parseLines :: [String] -> [Line]
parseLines = map str2line
  where
    str2line :: String -> Line
    str2line str =
      let [[x1, y1], [x2, y2]] = map (map read . splitOn ",") (splitOn " -> " str) :: [[Int]]
       in Line (y1, x1) (y2, x2) -- x is column and y is row

overlaps :: Int -> OceanFloor -> Int
overlaps n (OceanFloor ops) = length $ filter (\(OceanPoint _ n') -> n' >= n) ops
