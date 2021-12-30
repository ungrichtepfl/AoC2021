module Main where

import Day1
import Day2
import Day3
import Day4
import Day5

-- Inputs

inputFileDay1 :: FilePath
inputFileDay1 = "input/1_sonar-sweep.txt"

inputFileTestDay1 :: FilePath
inputFileTestDay1 = "input/1_sonar-sweep-test.txt"

inputFileDay2 :: FilePath
inputFileDay2 = "input/2_submarine-trajectory.txt"

inputFileTestDay2 :: FilePath
inputFileTestDay2 = "input/2_submarine-trajectory-test.txt"

inputFileDay3 :: FilePath
inputFileDay3 = "input/3_gamma-rates.txt"

inputFileTestDay3 :: FilePath
inputFileTestDay3 = "input/3_gamma-rates-test.txt"

inputFileDay4 :: FilePath
inputFileDay4 = "input/4_bingo.txt"

inputFileTestDay4 :: FilePath
inputFileTestDay4 = "input/4_bingo-test.txt"

inputFileDay5 :: FilePath
inputFileDay5 = "input/5_hydro.txt"

inputFileTestDay5 :: FilePath
inputFileTestDay5 = "input/5_hydro-test.txt"

-- Compute solutions to all problems:
main :: IO ()
main = do
  -- Day 1
  putStrLn "Start Day 1"
  solD1P1 <- day1Part1 inputFileDay1
  solD1P2 <- day1Part2 inputFileDay1
  putStr $ unlines ["Solution Day 1 part 1:" ++ show solD1P1, "Solution Day 1 part 2: " ++ show solD1P2]

  -- Day 2
  putStrLn "Start Day 2"
  solD2P1 <- day2Part1 inputFileDay2
  putStrLn $ "Solution to Day 2 part 1 is: " ++ show solD2P1
  solD2P2 <- day2Part2 inputFileDay2
  putStrLn $ "Solution to Day 2 part 2 is: " ++ show solD2P2

  -- Day 3
  putStrLn "Start Day 3"
  solD3P1 <- day3Part1 inputFileDay3
  putStrLn $ "Solution to Day 3 part 1 is: " ++ show solD3P1

  solD3P2 <- day3Part2 inputFileDay3
  putStrLn $ "Solution to Day 3 part 2 is: " ++ show solD3P2

  -- Day 4
  putStrLn "Start Day 4"
  aolD4P1 <- day4Part1 inputFileDay4
  putStrLn $ "Solution to Day 4 part 1 is: " ++ show aolD4P1

  aolD4P2 <- day4Part2 inputFileDay4
  putStrLn $ "Solution to Day 4 part 2 is: " ++ show aolD4P2

  -- Day 5
  putStrLn "Start Day 5"
  aolD5P1 <- day5Part1 inputFileDay5
  putStrLn $ "Solution to Day 5 part 1 is: " ++ show aolD5P1

  aolD5P2 <- day5Part2 inputFileDay5
  putStrLn $ "Solution to Day 5 part 2 is: " ++ show aolD5P2
