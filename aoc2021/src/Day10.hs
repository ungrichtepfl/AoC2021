module Day10
  ( day10Part1,
    day10Part2,
  )
where

import Control.Monad (foldM)
import Data.List (foldl')

day10Part1 :: FilePath -> IO Int
day10Part1 fp = do
  contents <- readFile fp
  print . parseInputs $ contents
  return $ -1

day10Part2 :: FilePath -> IO Int
day10Part2 fp = do
  contents <- readFile fp
  return $ -1

type Chunk = String

type Stack = Int

data Brackets = Brackets
  { parenthesis :: Stack,
    square :: Stack,
    curly :: Stack,
    angle :: Stack
  }
  deriving (Show)

checkChunk :: Chunk -> [(Char, Char)]
checkChunk c = snd $ foldl' manipulateStack (Brackets 0 0 0 0, []) c

manipulateStack :: (Brackets, [(Char, Char)]) -> Char -> (Brackets, [(Char, Char)])
manipulateStack (b@(Brackets p s c a), unmached) x = case x of
  '(' -> (b {parenthesis = push p}, unmached)
  '[' -> (b {square = push s}, unmached)
  '{' -> (b {curly = push c}, unmached)
  '<' -> (b {angle = push a}, unmached)
  ')' -> case pop p of
    Just p' -> (b {parenthesis = p'}, unmached)
    Nothing -> (b, (getExpected b, ')') : unmached)
  ']' -> case pop p of
    Just s' -> (b {square = s'}, unmached)
    Nothing -> (b, (getExpected b, ']') : unmached)
  '}' -> case pop p of
    Just c' -> (b {curly = c'}, unmached)
    Nothing -> (b, (getExpected b, '}') : unmached)
  '>' -> case pop p of
    Just a' -> (b {angle = a'}, unmached)
    Nothing -> (b, (getExpected b, '>') : unmached)
  _ -> error "Wrong bracket."
  where
    getExpected (Brackets p' s' c' a')
      | p' > 0 = ')'
      | s' > 0 = ']'
      | c' > 0 = '}'
      | a' > 0 = '>'
      | otherwise = error "Unreachable"

pop :: Stack -> Maybe Stack
pop s = if s == 0 then Nothing else Just $ s - 1

push :: Stack -> Stack
push = (1 +)

parseInputs :: String -> [Chunk]
parseInputs = lines
