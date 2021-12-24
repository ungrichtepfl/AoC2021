module Day4
  ( day4Part1,
  )
where

import qualified Data.List
import Data.List.Split (splitOn)
import Data.Matrix
import Data.Maybe

day4Part1 :: FilePath -> IO Int
day4Part1 fp = do
  contents <- readFile fp
  let (bingoNumbers, bingoBoards) = parseBingoFile contents
  let (winNum, winBoard) = case getWinner bingoNumbers bingoBoards of
        Just (n, b) -> (n, b)
        Nothing -> error "No winner found"
  putStrLn $ "Winner board is:\n" ++ show (board winBoard)
  putStrLn $ "Marked numbers:\n" ++ show (marked winBoard)
  putStrLn $ "Winner number: " ++ show winNum
  return $ winNum * sumOfUnmarked winBoard

data BingoBoard = BingoBoard
  { board :: Matrix Int,
    marked :: Matrix Bool
  }
  deriving (Show)

unmarked :: BingoBoard -> Matrix Bool
unmarked (BingoBoard _ m) = mapPos (\_ x -> not x) m

type BingoNumber = Int

type BingoNumbers = [BingoNumber]

parseBingoFile :: String -> (BingoNumbers, [BingoBoard])
parseBingoFile ip =
  let bingoNumbersStr : bingoBoardsStr = lines ip
      bingoNumbers = map read $ splitOn "," bingoNumbersStr :: [Int]
      bingoBoards = parseBingoBoards bingoBoardsStr
   in (bingoNumbers, bingoBoards)

parseBingoBoards :: [String] -> [BingoBoard]
parseBingoBoards [] = []
parseBingoBoards (_ : l1 : l2 : l3 : l4 : l5 : rest) =
  parseBingoBoard (l1 : l2 : l3 : l4 : [l5]) : parseBingoBoards rest

parseBingoBoard :: [String] -> BingoBoard
parseBingoBoard bbs =
  let bbls = map words bbs :: [[String]]
      bbl = map (map read) bbls :: [[Int]]
      b = fromLists bbl
      m = matrix (nrows b) (ncols b) (\_ -> False)
   in BingoBoard b m

sumOfMarked :: BingoBoard -> Int
sumOfMarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ marked bb)

sumOfUnmarked :: BingoBoard -> Int
sumOfUnmarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ unmarked bb)

getWinner :: BingoNumbers -> [BingoBoard] -> Maybe (BingoNumber, BingoBoard)
getWinner [] _ = Nothing
getWinner (num : rest) bbs =
  let mbbs = markBoards num bbs
      bbWon = map checkIfWon mbbs
      idxWon = Data.List.elemIndex True bbWon
   in case idxWon of
        Just idx -> Just (num, mbbs !! idx)
        Nothing -> getWinner rest mbbs

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
