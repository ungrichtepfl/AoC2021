module Day11
  ( day11Part1,
    day11Part2,
  )
where

import qualified Data.List
import Data.Matrix

day11Part1 :: FilePath -> IO Int
day11Part1 fp = do
  contents <- readFile fp
  let startOct = parseInputs contents
  print startOct
  print $ updateEnergy . updateEnergy $ startOct
  return $ -1

type Energy = Int

type Octopuses = Matrix Energy

increaseBy1 :: Octopuses -> Octopuses
increaseBy1 oct = elementwiseUnsafe (+) oct (fromList (nrows oct) (ncols oct) (replicate (nrows oct * ncols oct) 1))

initialFlashing :: Octopuses -> [(Int, Int)]
initialFlashing = elemIndicesMat 10 . increaseBy1

updateEnergy :: Octopuses -> Octopuses
updateEnergy oct = foldr updateAround oct $ initialFlashing oct

updateAround :: (Int, Int) -> Octopuses -> Octopuses
updateAround (i, j) oct = foldr go oct [(i + 1, j), (i - 1, j), (i, j + 1), (i + 1, j), (i + 1, j + 1), (i -1, j -1), (i + 1, j -1), (i -1, j + 1)]
  where
    go :: (Int, Int) -> Octopuses -> Octopuses
    go idx@(i', j') accumOct
      | i' < 1 || i' > nrows accumOct || j' < 1 || j' > ncols accumOct = accumOct
      | accumOct ! idx == 10 = accumOct
      | accumOct ! idx == 9 = updateAround idx $ setElem 10 idx accumOct
      | otherwise = setElem (accumOct ! idx + 1) idx accumOct

countFlashing :: Octopuses -> [(Int, Int)] -> Int
countFlashing oct flashing = undefined

day11Part2 :: FilePath -> IO Int
day11Part2 fp = do
  contents <- readFile fp
  return $ -1

parseInputs :: String -> Octopuses
parseInputs = fromLists . map (map (read . (: []))) . lines

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
