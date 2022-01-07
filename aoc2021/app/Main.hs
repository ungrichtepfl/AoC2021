module Main where

{-
This file will be automatically updated with the function `new_day day_number input_file_name`.
Be careful when updating the main function by hand. It may break the function `new_day`.
-}

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import System.Environment

-- Inputs

inputDay01 :: FilePath
inputDay01 = "input/01_sonar-sweep.txt"

sampleDay01 :: FilePath
sampleDay01 = "input/01_sonar-sweep-test.txt"

inputDay02 :: FilePath
inputDay02 = "input/02_submarine-trajectory.txt"

sampleDay02 :: FilePath
sampleDay02 = "input/02_submarine-trajectory-test.txt"

inputDay03 :: FilePath
inputDay03 = "input/03_gamma-rates.txt"

sampleDay03 :: FilePath
sampleDay03 = "input/03_gamma-rates-test.txt"

inputDay04 :: FilePath
inputDay04 = "input/04_bingo.txt"

sampleDay04 :: FilePath
sampleDay04 = "input/04_bingo-test.txt"

inputDay05 :: FilePath
inputDay05 = "input/05_hydro.txt"

sampleDay05 :: FilePath
sampleDay05 = "input/05_hydro-test.txt"

inputDay06 :: FilePath
inputDay06 = "input/06_lanternfish.txt"

sampleDay06 :: FilePath
sampleDay06 = "input/06_lanternfish-test.txt"

inputDay07 :: FilePath
inputDay07 = "input/07_crab_submarine.txt"

sampleDay07 :: FilePath
sampleDay07 = "input/07_crab_submarine-test.txt"

inputDay08 :: FilePath
inputDay08 = "input/08_seven_segment_displays.txt"

sampleDay08 :: FilePath
sampleDay08 = "input/08_seven_segment_displays-test.txt"

inputDay09 :: FilePath
inputDay09 = "input/09_low_points.txt"

sampleDay09 :: FilePath
sampleDay09 = "input/09_low_points-test.txt"

inputDay10 :: FilePath
inputDay10 = "input/10_syntax_scoring.txt"

sampleDay10 :: FilePath
sampleDay10 = "input/10_syntax_scoring-test.txt"

type Solution = (IO Int, IO Int)

solutions :: [Solution]
solutions =
  [ ( day01Part1 inputDay01,
      day01Part2 inputDay01
    ),
    ( day02Part1 inputDay02,
      day02Part2 inputDay02
    ),
    ( day03Part1 inputDay03,
      day03Part2 inputDay03
    ),
    ( day04Part1 inputDay04,
      day04Part2 inputDay04
    ),
    ( day05Part1 inputDay05,
      day05Part2 inputDay05
    ),
    ( day06Part1 inputDay06,
      day06Part2 inputDay06
    ),
    ( day07Part1 inputDay07,
      day07Part2 inputDay07
    ),
    ( day08Part1 inputDay08,
      day08Part2 inputDay08
    ),
    ( day09Part1 inputDay09,
      day09Part2 inputDay09
    ),
    ( day10Part1 sampleDay10,
      day10Part2 sampleDay10
    )
  ]

-- | Print all the solutions in the solutions list
printSolutions :: IO ()
printSolutions = printRec (1, solutions)
  where
    printRec :: (Int, [Solution]) -> IO ()
    printRec (_, []) = pure ()
    printRec (i, x : xs) = printSolution (i, x) >> printRec (i + 1, xs)

-- | Print part 1 and part 2 of one solution (of a day)
printSolution :: (Int, Solution) -> IO ()
printSolution (i, (part1, part2)) =
  putStrLn ("Day " ++ show i ++ ", Part 1:") >> part1 >>= print >> putStrLn ("Day " ++ show i ++ ", Part 2:") >> part2 >>= print

-- | Print part 1 of one solution (of a day)
printSolution1 :: (Int, Solution) -> IO ()
printSolution1 (i, (part1, _)) = putStrLn ("Day " ++ show i ++ ", Part 1:") >> part1 >>= print

-- | Print part 2 of one solution (of a day)
printSolution2 :: (Int, Solution) -> IO ()
printSolution2 (i, (_, part2)) = putStrLn ("Day " ++ show i ++ ", Part 2:") >> part2 >>= print

-- | Entry Point
main :: IO ()
main = getArgs >>= mainArgs

-- | run different solutions based on list of arguments given.
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
    putStrLn $ "Usage: stack run " ++ pName ++ " [day [part]]."
    putStrLn $ "day has to be between 1 and " ++ show (length solutions) ++ " and part either 1 or 2."
    putStrLn "Not specifing any arguments will print all the available solutions."
