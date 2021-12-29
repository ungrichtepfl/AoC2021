module Day4
  ( day4Part1,
    day4Part2,
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

day4Part2 :: FilePath -> IO Int
day4Part2 fp = do
  contents <- readFile fp
  let (bingoNumbers, bingoBoards) = parseBingoFile contents
  let (winNum, winBoard) = case getLastWinner bingoNumbers bingoBoards of
        Just (n, b) -> (n, b)
        Nothing -> error "No winner found"
  putStrLn $ "Last winner board is:\n" ++ show (board winBoard)
  putStrLn $ "Marked numbers:\n" ++ show (marked winBoard)
  putStrLn $ "Winner number: " ++ show winNum
  return $ winNum * sumOfUnmarked winBoard

data BingoBoard = BingoBoard
  { -- | Contains the 5 x 5 Bingo board
    board :: Matrix Int,
    -- | State of marked number, 5 x 5 bool grid (True if marked)
    marked :: Matrix Bool
  }
  deriving (Show)

-- | Get the state of all unmarked numbers, 5 x 5 bool grid (True if unmarked)
unmarked :: BingoBoard -> Matrix Bool
unmarked (BingoBoard _ m) = mapPos (\_ x -> not x) m

type BingoNumber = Int

type BingoNumbers = [BingoNumber]

-- | File as string input returns a list of Bingo numbers and the Bingo Boards
parseBingoFile :: String -> (BingoNumbers, [BingoBoard])
parseBingoFile ip =
  let bingoNumbersStr : bingoBoardsStr = lines ip
      bingoNumbers = map read $ splitOn "," bingoNumbersStr :: [Int]
      bingoBoards = parseBingoBoards bingoBoardsStr
   in (bingoNumbers, bingoBoards)

-- | Input of bingo file lines without the first line
parseBingoBoards :: [String] -> [BingoBoard]
parseBingoBoards [] = []
parseBingoBoards (_ : l1 : l2 : l3 : l4 : l5 : rest) =
  parseBingoBoard (l1 : l2 : l3 : l4 : [l5]) : parseBingoBoards rest

-- | 5 lines to one Bingo board
parseBingoBoard :: [String] -> BingoBoard
parseBingoBoard bbs =
  let bbls = map words bbs :: [[String]]
      bbl = map (map read) bbls :: [[Int]]
      b = fromLists bbl
      m = matrix (nrows b) (ncols b) (\_ -> False)
   in BingoBoard b m

-- | Returns the sum of all marked numbers on a Bingo Board
sumOfMarked :: BingoBoard -> Int
sumOfMarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ marked bb)

-- | Returns the sum of all unmarked numbers on a Bingo board
sumOfUnmarked :: BingoBoard -> Int
sumOfUnmarked bb = sum $ toList $ elementwise (*) (board bb) (matBool2Int $ unmarked bb)

-- | Returns a list of all the winning Bingo numbers and the corresponding winner Board from first to last winner.
getWinnerBoards :: BingoNumbers -> [BingoBoard] -> ([Int], [[BingoBoard]])
getWinnerBoards [] _ = ([], [])
getWinnerBoards (num : rest) bbs =
  let mbbs = markBoards num bbs
      bbsWon = map checkIfWon mbbs
      currWinBoards = getIfTrue bbsWon mbbs
      currLoserBoards = getIfTrue (map not bbsWon) mbbs
      (winNums, winBoards) = getWinnerBoards rest currLoserBoards
   in case currWinBoards of
        [] -> (winNums, winBoards)
        _ -> (num : winNums, currWinBoards : winBoards)

-- | Returns the last winner of the Bingo game
getLastWinner :: BingoNumbers -> [BingoBoard] -> Maybe (BingoNumber, BingoBoard)
getLastWinner nums bbs =
  case getWinnerBoards nums bbs of
    (_, []) -> Nothing
    (winNum, winBoards) -> Just (last winNum, last $ last winBoards)

-- | Filter a list based on a bool list
getIfTrue :: [Bool] -> [a] -> [a]
getIfTrue [] xs = xs
getIfTrue (b : bs) (x : xs)
  | b = x : getIfTrue bs xs
  | otherwise = getIfTrue bs xs

-- | Returns the winner of the Bingo game
getWinner :: BingoNumbers -> [BingoBoard] -> Maybe (BingoNumber, BingoBoard)
getWinner [] _ = Nothing
getWinner (num : rest) bbs =
  let mbbs = markBoards num bbs
      bbWon = map checkIfWon mbbs
      idxWon = Data.List.elemIndex True bbWon
   in case idxWon of
        Just idx -> Just (num, mbbs !! idx)
        Nothing -> getWinner rest mbbs

-- | Returns True if the Board is in a winning state
checkIfWon :: BingoBoard -> Bool
checkIfWon (BingoBoard _ m) =
  let mi = matBool2Int m
      colSums = multStd mi $ matrix (nrows mi) 1 (\_ -> 1)
      rowSums = multStd (transpose mi) $ matrix (ncols mi) 1 (\_ -> 1)
      wonCol = isJust $ elemInMat (ncols mi) colSums
      wonRow = isJust $ elemInMat (nrows mi) rowSums
   in wonCol || wonRow

-- | Mark a board if number is on the board else do nothing
markNumber :: Int -> BingoBoard -> BingoBoard
markNumber num ob@BingoBoard {board = b, marked = m} =
  let pos = elemInMat num b
   in if isJust pos
        then BingoBoard b (setElem True (fromJust pos) m)
        else ob

-- | Mark a list of Boards with the called number
markBoards :: Int -> [BingoBoard] -> [BingoBoard]
markBoards num = map (markNumber num)

-- | Returns the indices of an element in a Matrix (indices start from 1)
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

-- | Transforms a bool matrix to an int matrix (False -> 0, True -> 1)
matBool2Int :: Matrix Bool -> Matrix Int
matBool2Int = mapPos (\_ x -> fromEnum x)
