module Main where

import Day1
import Day2

inputFileDay1 :: FilePath
inputFileDay1 = "input/sonar-sweep.txt"

inputFileTestDay1 :: FilePath
inputFileTestDay1 = "input/sonar-sweep-test.txt"

inputFileDay2 :: FilePath
inputFileDay2 = "input/submarine-trajectory.txt"

inputFileTestDay2 :: FilePath
inputFileTestDay2 = "input/submarine-trajectory-test.txt"

main :: IO ()
main = do
  -- Day 1
  solD1P1 <- day1Part1 inputFileDay1
  solD1P2 <- day1Part2 inputFileDay1
  putStr $ unlines ["Solution Day 1 part 1: " ++ show solD1P1, "Solution Day 1 part 2: " ++ show solD1P2]

  -- Day 2
  solD2P1 <- day2Part1 inputFileDay2
  putStrLn $ "Solution to Day 2 part 1 is: " ++ show solD2P1
  solD2P2 <- day2Part2 inputFileDay2
  putStrLn $ "Solution to Day 2 part 2 is: " ++ show solD2P2
