module Main where

import Day2

inputFile :: FilePath
inputFile = "submarine-trajectory.txt"

inputFileTest :: FilePath
inputFileTest = "submarine-trajectory-test.txt"

main :: IO ()
main = do
  solution1 <- day2Part1 inputFile
  putStrLn $ "Solution to part 1 is: " ++ show solution1
  solution2 <- day2Part2 inputFile
  putStrLn $ "Solution to part 2 is: " ++ show solution2
