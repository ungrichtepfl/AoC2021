module Main where

import Day1

inputFile :: FilePath
inputFile = "sonar-sweep.txt"

inputFileTest :: FilePath
inputFileTest = "sonar-sweep-test.txt"

main :: IO ()
main = do
  solution1 <- day1Part1 inputFile
  solution2 <- day1Part2 inputFile
  putStr $ unlines ["Solution part 1: " ++ show solution1, "Solution part 1: " ++ show solution2]
