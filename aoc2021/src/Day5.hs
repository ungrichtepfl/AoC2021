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
  let finalFloor = markLines (initFloor ls) ls
  --  putStrLn $ "Final floor:\n" ++ show finalFloor
  return $ overlaps 2 finalFloor

day5Part2 :: FilePath -> IO Int
day5Part2 = undefined

newtype OceanFloor = OceanFloor (Matrix Int) deriving (Show)

data Line = Line
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show)

initFloor :: [Line] -> OceanFloor
initFloor ls = OceanFloor $ zero (maxRow + 1) (maxCol + 1)
  where
    (maxRow, maxCol) = getMax ls
    getMax :: [Line] -> (Int, Int)
    getMax [] = (-1, -1)
    getMax ((Line (x1, y1) (x2, y2)) : xys) = let (nx, ny) = getMax xys in (maximum [nx, x1, x2], maximum [ny, y1, y2])

markLines :: OceanFloor -> [Line] -> OceanFloor
markLines = foldl' markLine

markLine :: OceanFloor -> Line -> OceanFloor
markLine (OceanFloor m) l = OceanFloor $ mapPos (\(x, y) a -> if (x -1, y -1) `elem` conns then a + 1 else a) m
  where
    conns = connection l

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
overlaps n (OceanFloor m) = length $ filter (>= n) (toList m)
