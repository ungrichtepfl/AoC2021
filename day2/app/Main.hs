module Main where

import Day2

inputFile :: FilePath
inputFile = "submarine-trajectory.txt"

inputFileTest :: FilePath
inputFileTest = "submarine-trajectory-test.txt"

main :: IO ()
main = do
  solution1 <- day2Part1 inputFile
  --  solution2 <- day1Part2 inputFile
  print solution1
