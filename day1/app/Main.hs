module Main where

import Day1

inputFile :: FilePath
inputFile = "sonar-sweep.txt"

inputFileTest :: FilePath
inputFileTest = "sonar-sweep-test.txt"

main :: IO ()
main = do
  solution <- day1Part1 inputFile
  print solution
