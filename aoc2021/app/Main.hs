module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import System.Environment

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

inputFileDay6 :: FilePath
inputFileDay6 = "input/6_lanternfish.txt"

inputFileTestDay6 :: FilePath
inputFileTestDay6 = "input/6_lanternfish-test.txt"

inputFileDay7 :: FilePath
inputFileDay7 = "input/7_crab_submarine.txt"

inputFileTestDay7 :: FilePath
inputFileTestDay7 = "input/7_crab_submarine-test.txt"

type Solution = (IO Int, IO Int)

solutions :: [Solution]
solutions =
  [ ( day1Part1 inputFileDay1,
      day1Part2 inputFileDay1
    ),
    ( day2Part1 inputFileDay2,
      day2Part2 inputFileDay2
    ),
    ( day3Part1 inputFileDay3,
      day3Part2 inputFileDay3
    ),
    ( day4Part1 inputFileDay4,
      day4Part2 inputFileDay4
    ),
    ( day5Part1 inputFileDay5,
      day5Part2 inputFileDay5
    ),
    ( day6Part1 inputFileDay6,
      day6Part2 inputFileDay6
    ),
    ( day7Part1 inputFileTestDay7,
      day7Part2 inputFileTestDay7
    )
  ]

-- Compute solutions to all problems:
main :: IO ()
main = getArgs >>= mainArgs

mainArgs :: [String] -> IO ()
mainArgs args
  | null args = printSolutions
  | length args == 1 && head args `elem` map show ([1 .. length solutions] :: [Int]) =
    let day = read $ head args :: Int
     in printSolution (day, solutions !! (day - 1))
  | length args == 2 && head args `elem` map show ([1 .. length solutions] :: [Int]) && last args `elem` ["1", "2"] =
    let day = read $ head args :: Int
        part = read $ last args :: Int
     in if part == 1 then printSolution1 (day, solutions !! (day - 1)) else printSolution2 (day, solutions !! (day -1))
  | otherwise = do
    pName <- getProgName
    putStrLn $ "Usage: stack run " ++ pName ++ " [day_number [part]]."
    putStrLn $ "day_number has between 1 and " ++ show (length solutions) ++ " and part either 1 or 2."
    putStrLn "Not specifing any arguments will print all the available solutions."

printSolutions :: IO ()
printSolutions = printRec (1, solutions)
  where
    printRec :: (Int, [Solution]) -> IO ()
    printRec (_, []) = pure ()
    printRec (i, x : xs) = printSolution (i, x) >> printRec (i + 1, xs)

printSolution :: (Int, Solution) -> IO ()
printSolution (i, (part1, part2)) =
  putStrLn ("Day " ++ show i ++ ", Part 1:") >> part1 >>= print >> putStrLn ("Day " ++ show i ++ ", Part 2:") >> part2 >>= print

printSolution1 :: (Int, Solution) -> IO ()
printSolution1 (i, (part1, _)) = putStrLn ("Day " ++ show i ++ ", Part 1:") >> part1 >>= print

printSolution2 :: (Int, Solution) -> IO ()
printSolution2 (i, (_, part2)) = putStrLn ("Day " ++ show i ++ ", Part 2:") >> part2 >>= print
