module Day4
  ( day4Part1,
  )
where

import qualified Data.List
import Data.Matrix
import Data.Maybe

day4Part1 :: FilePath -> IO Int
day4Part1 _ = y

y :: IO Int
y = undefined

data BingoBoard = BingoBoard
  { board :: Matrix Int,
    marked :: Matrix Bool
  }
  deriving (Show, Eq)

unmarked :: BingoBoard -> Matrix Bool
unmarked (BingoBoard _ m) = mapPos (\_ x -> not x) m

sumOfMarked :: BingoBoard -> Int
sumOfMarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ marked bb)

sumOfUnmarked :: BingoBoard -> Int
sumOfUnmarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ unmarked bb)

checkIfWon :: BingoBoard -> Bool
checkIfWon (BingoBoard _ m) =
  let mi = matBool2Int m
      colSums = multStd mi $ matrix (nrows mi) 1 (\_ -> 1)
      rowSums = multStd (transpose mi) $ matrix (ncols mi) 1 (\_ -> 1)
      wonCol = isJust $ elemInMat (ncols mi) colSums
      wonRow = isJust $ elemInMat (nrows mi) rowSums
   in wonCol || wonRow

markNumber :: Int -> BingoBoard -> BingoBoard
markNumber num ob@BingoBoard {board = b, marked = m} =
  let pos = elemInMat num b
   in if isJust pos
        then BingoBoard b (setElem True (fromJust pos) m)
        else ob

markBoards :: Int -> [BingoBoard] -> [BingoBoard]
markBoards num = map (markNumber num)

elemInMat :: (Eq a) => a -> Matrix a -> Maybe (Int, Int)
elemInMat x m =
  let ml = toList m
      lim = Data.List.elemIndex x ml
   in if isNothing lim
        then Nothing
        else
          let li = fromJust lim
              i = li `div` ncols m
              j = li - i * ncols m
           in Just (i + 1, j + 1) -- indices start at 1

matBool2Int :: Matrix Bool -> Matrix Int
matBool2Int = mapPos (\_ x -> fromEnum x)
