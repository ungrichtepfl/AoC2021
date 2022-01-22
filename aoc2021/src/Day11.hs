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
  let res = updateAndCountNTimes 100 (startOct, 0)
  print $ fst res
  return $ snd res

day11Part2 :: FilePath -> IO Int
day11Part2 fp = do
  contents <- readFile fp
  let startOct = parseInputs contents
  let (syncedOct, numberOfUpdates) = syncedAt (startOct, 0)
  print startOct
  print syncedOct
  return numberOfUpdates

type Energy = Int

type Octopuses = Matrix Energy

increaseBy1 :: Octopuses -> Octopuses
increaseBy1 = mapPos (\_ e -> if e == 9 then 0 else e + 1)

initialFlashing :: Octopuses -> [(Int, Int)]
initialFlashing = elemIndicesMat 0

updateAndCountNTimes :: Int -> (Octopuses, Int) -> (Octopuses, Int)
updateAndCountNTimes n = foldr (.) id (replicate n updateAndCount)

syncedAt :: (Octopuses, Int) -> (Octopuses, Int)
syncedAt (oct, i) = if length (elemIndicesMat 0 oct) == ncols oct * nrows oct then (oct, i) else syncedAt (updateEnergy oct, i + 1)

updateAndCount :: (Octopuses, Int) -> (Octopuses, Int)
updateAndCount (oct, count) = let updatedOct = updateEnergy oct in (updatedOct, count + length (elemIndicesMat 0 updatedOct))

updateEnergy :: Octopuses -> Octopuses
updateEnergy oct = let octUpdated = increaseBy1 oct in foldr updateAround octUpdated $ initialFlashing octUpdated

updateAround :: (Int, Int) -> Octopuses -> Octopuses
updateAround (i, j) oct = foldr go oct [(i + 1, j), (i - 1, j), (i, j + 1), (i, j -1), (i + 1, j + 1), (i -1, j -1), (i + 1, j -1), (i -1, j + 1)]
  where
    go :: (Int, Int) -> Octopuses -> Octopuses
    go idx@(i', j') accumOct
      | i' < 1 || i' > nrows accumOct || j' < 1 || j' > ncols accumOct = accumOct
      | accumOct ! idx == 0 = accumOct
      | accumOct ! idx == 9 = updateAround idx $ setElem 0 idx accumOct
      | otherwise = setElem (accumOct ! idx + 1) idx accumOct

countFlashing :: Octopuses -> [(Int, Int)] -> Int
countFlashing oct flashing = undefined

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
