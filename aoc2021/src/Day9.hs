module Day9
  ( day9Part1,
    day9Part2,
  )
where

import Data.List (sortBy)
import qualified Data.List
import Data.Matrix

--import qualified Debug.Trace as DB

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
  let mat = parseInput contents
  let locMinIdxs = (elemIndicesMat True . localMinima1) mat
  --  print locMinIdxs
  let basins = sortBy (flip compare) $ map (reportNum mat) locMinIdxs
  --  print basins
  (return . product . take 3) basins

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

reportNum :: Matrix Int -> (Int, Int) -> Int
reportNum mat start = fst $ recursiveSearch [] mat start

recursiveSearch :: [(Int, Int)] -> Matrix Int -> (Int, Int) -> (Int, [(Int, Int)])
recursiveSearch alreadyVisited mat currIdx@(i, j)
  | currIdx `elem` alreadyVisited || i == 0 || j == 0 || i == nrows mat + 1 || j == ncols mat + 1 = (0, alreadyVisited)
  | otherwise =
    if (mat ! currIdx) == 9
      then (0, currIdx : alreadyVisited)
      else
        let (s1, v1) = recursiveSearch (currIdx : alreadyVisited) mat (i - 1, j)
            (s2, v2) = recursiveSearch v1 mat (i + 1, j)
            (s3, v3) = recursiveSearch v2 mat (i, j - 1)
            (s4, v4) = recursiveSearch v3 mat (i, j + 1)
         in (1 + s1 + s2 + s3 + s4, v4)

elemIndicesMat :: (Eq a) => a -> Matrix a -> [(Int, Int)]
elemIndicesMat x m =
  let l = toList m
      idxs = Data.List.elemIndices x l
   in map transform2tuple idxs
  where
    transform2tuple idx =
      let i = idx `div` ncols m
          j = idx - i * ncols m
       in (i + 1, j + 1) -- indices start at 1
