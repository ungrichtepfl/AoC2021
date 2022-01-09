module Day10
  ( day10Part1,
    day10Part2,
  )
where

import Data.List (sort)

day10Part1 :: FilePath -> IO Int
day10Part1 fp = do
  contents <- readFile fp
  let chunks = parseInputs contents
  let checkedChunks = map (checkChunk []) chunks
  printErrors $ zip chunks checkedChunks
  return $ calculateErrorScore checkedChunks

day10Part2 :: FilePath -> IO Int
day10Part2 fp = do
  contents <- readFile fp
  let chunks = parseInputs contents
  let checkedChunks = map (checkChunk []) chunks
  let incomplete = discardCorrupted chunks checkedChunks
  let completed = map complete incomplete
  --  printComplete completed
  let scores = map calculateCompletionScore completed
  --  print $ sort scores
  return $ sort scores !! (length scores `div` 2)

type Bracket = Char

type Chunk = [Bracket]

type Stack = [Bracket]

open :: [Bracket]
open =
  [ '{',
    '(',
    '<',
    '['
  ]

closed :: [Bracket]
closed =
  [ '}',
    ')',
    '>',
    ']'
  ]

calculateCompletionScore :: (Chunk, Chunk) -> Int
calculateCompletionScore = calcScore 0 . snd
  where
    calcScore :: Int -> Chunk -> Int
    calcScore cs [] = cs
    calcScore cs (comp : rest) =
      let toAdd = case comp of
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            _ -> error $ "Wrong bracket" ++ show comp
       in calcScore (cs * 5 + toAdd) rest

complete :: (Chunk, Stack) -> (Chunk, Chunk)
complete (c, s) = (c, map flipBracket s)

printComplete :: [(Chunk, Chunk)] -> IO ()
printComplete [] = return ()
printComplete ((c, comp) : rest) = putStrLn (show c ++ " - Complete by adding " ++ show comp) >> printComplete rest

discardCorrupted :: [Chunk] -> [Either (Bracket, Bracket) Stack] -> [(Chunk, Stack)]
discardCorrupted [] _ = []
discardCorrupted (c : cs) ((Right s) : ss) = (c, s) : discardCorrupted cs ss
discardCorrupted (_ : cs) ((Left _) : ss) = discardCorrupted cs ss

calculateErrorScore :: [Either (Bracket, Bracket) Stack] -> Int
calculateErrorScore [] = 0
calculateErrorScore ((Right _) : rest) = calculateErrorScore rest
calculateErrorScore ((Left (_, found)) : rest) = case found of
  ')' -> 3 + calculateErrorScore rest
  ']' -> 57 + calculateErrorScore rest
  '}' -> 1197 + calculateErrorScore rest
  '>' -> 25137 + calculateErrorScore rest
  _ -> error $ "Wrong kind of bracket: " ++ show found

printErrors :: [(Chunk, Either (Bracket, Bracket) Stack)] -> IO ()
printErrors [] = return ()
printErrors ((c, Left (expected, found)) : rest) = putStrLn (show c ++ " - Expected " ++ show expected ++ ", but found " ++ show found ++ " instead.") >> printErrors rest
printErrors (_ : rest) = printErrors rest

checkChunk :: Stack -> Chunk -> Either (Bracket, Bracket) Stack
checkChunk s [] = Right s
checkChunk s (b : bs)
  | b `elem` open = checkChunk (push s b) bs
  | b `elem` closed = do
    ns <- pop s b
    checkChunk ns bs
  | otherwise = error $ "Wrong kind of bracket. " ++ show b

pop :: Stack -> Bracket -> Either (Bracket, Bracket) Stack
pop [] found = Left (flipBracket found, found)
pop (toclose : s) found
  | flipBracket toclose == found = Right s
  | otherwise = Left (flipBracket toclose, found)

flipBracket :: Bracket -> Bracket
flipBracket c = case c of
  '(' -> ')'
  ')' -> '('
  '{' -> '}'
  '}' -> '{'
  '<' -> '>'
  '>' -> '<'
  '[' -> ']'
  ']' -> '['
  _ -> error $ "Wrong bracket: " ++ show c

push :: Stack -> Bracket -> Stack
push s = (: s)

parseInputs :: String -> [Chunk]
parseInputs = lines
