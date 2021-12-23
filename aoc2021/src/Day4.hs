module Day4
  ( day4Part1,
  )
where

import Data.List
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

markNumber :: Int -> BingoBoard -> BingoBoard
markNumber num ob@BingoBoard {board = b, marked = m} =
  let pos = elemInMat num b
   in if isJust pos
        then BingoBoard b (setElem True (fromJust pos) m)
        else ob

elemInMat :: (Eq a) => a -> Matrix a -> Maybe (Int, Int)
elemInMat x m =
  let ml = toList m
      lim = elemIndex x ml
   in if isNothing lim
        then Nothing
        else
          let li = fromJust lim
              i = li `div` ncols m
              j = li - i * ncols m
           in Just (i + 1, j + 1) -- indices start at 1

matBool2Int :: Matrix Bool -> Matrix Int
matBool2Int = mapPos (\_ x -> fromEnum x)
