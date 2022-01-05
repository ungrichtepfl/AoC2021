module Day9
  ( day9Part1,
    day9Part2,
  )
where

import Data.Matrix

day9Part1 :: FilePath -> IO Int
day9Part1 fp = do
  contents <- readFile fp
  let mat = parseInput contents
  --  print mat
  let locMin = localMinima1 mat
  --  print locMin
  (return . sum . toList . elementwise (\a b -> (a + 1) * fromEnum b) mat) locMin

day9Part2 :: FilePath -> IO Int
day9Part2 fp = do
  contents <- readFile fp
  return (-1)

localMinima1 :: Matrix Int -> Matrix Bool
localMinima1 mat = mapPos checkAround mat
  where
    checkAround :: (Int, Int) -> Int -> Bool
    checkAround (i, j) x
      | i == 1 && j == 1 = x < (mat ! (i, j + 1)) && x < (mat ! (i + 1, j))
      | i == 1 && j == ncols mat = (mat ! (i, j -1)) > x && x < (mat ! (i + 1, j))
      | i == nrows mat && j == 1 = x < (mat ! (i, j + 1)) && (mat ! (i -1, j)) > x
      | i == nrows mat && j == ncols mat = (mat ! (i, j -1)) > x && (mat ! (i -1, j)) > x
      | i == 1 = (mat ! (i, j -1)) > x && x < (mat ! (i, j + 1)) && x < (mat ! (i + 1, j))
      | i == nrows mat = (mat ! (i, j -1)) > x && x < (mat ! (i, j + 1)) && (mat ! (i -1, j)) > x
      | j == 1 = x < (mat ! (i, j + 1)) && (mat ! (i -1, j)) > x && x < (mat ! (i + 1, j))
      | j == ncols mat = (mat ! (i, j -1)) > x && (mat ! (i -1, j)) > x && x < (mat ! (i + 1, j))
      | otherwise = (mat ! (i, j -1)) > x && x < (mat ! (i, j + 1)) && (mat ! (i -1, j)) > x && x < (mat ! (i + 1, j))

parseInput :: String -> Matrix Int
parseInput = fromLists . map (map (read . (: []))) . lines
