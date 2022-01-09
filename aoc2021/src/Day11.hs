module Day11
  ( day11Part1,
    day11Part2,
  )
where

import Data.Matrix

day11Part1 :: FilePath -> IO Int
day11Part1 fp = do
  contents <- readFile fp
  let startOct = parseInputs contents
  print startOct
  print $ increaseBy1 startOct
  return $ -1

type Octopuses = Matrix Int

increaseBy1 :: Octopuses -> Octopuses
increaseBy1 o = elementwiseUnsafe (+) o (fromList (nrows o) (ncols o) (replicate (nrows o * ncols o) 1))

day11Part2 :: FilePath -> IO Int
day11Part2 fp = do
  contents <- readFile fp
  return $ -1

parseInputs :: String -> Octopuses
parseInputs = fromLists . map (map (read . (: []))) . lines
