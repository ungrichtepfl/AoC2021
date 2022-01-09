module Day10
  ( day10Part1,
    day10Part2,
  )
where

day10Part1 :: FilePath -> IO Int
day10Part1 fp = do
  contents <- readFile fp
  let chunks = parseInputs contents
  let checkedChunks = map (checkChunk []) chunks
  --  printErrors checkedChunks
  return $ calculateScore checkedChunks

day10Part2 :: FilePath -> IO Int
day10Part2 fp = do
  contents <- readFile fp
  return $ -1

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

calculateScore :: [Either (Bracket, Bracket) Stack] -> Int
calculateScore [] = 0
calculateScore ((Right _) : rest) = calculateScore rest
calculateScore ((Left (_, found)) : rest) = case found of
  ')' -> 3 + calculateScore rest
  ']' -> 57 + calculateScore rest
  '}' -> 1197 + calculateScore rest
  '>' -> 25137 + calculateScore rest
  _ -> error $ "Wrong kind of bracket: " ++ show found

printErrors :: [Either (Bracket, Bracket) Stack] -> IO ()
printErrors [] = return ()
printErrors ((Left (expected, found)) : rest) = putStrLn ("Expected " ++ show expected ++ ", but found " ++ show found ++ " instead.") >> printErrors rest
printErrors ((Right _) : rest) = printErrors rest

checkChunk :: Stack -> Chunk -> Either (Bracket, Bracket) Stack
checkChunk s [] = Right s
checkChunk s (b : bs)
  | b `elem` open = checkChunk (push s b) bs
  | b `elem` closed = case pop s b of
    Right ns -> checkChunk ns bs
    ef -> ef
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
