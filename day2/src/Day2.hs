module Day2
  ( day2Part1,
  )
where

data SubAction = Up Int | Down Int | Forward Int deriving (Show)

day2Part1 :: FilePath -> IO Int
day2Part1 filePath = do
  contents <- readFile filePath
  let subActions = parseInput contents
  let (horizonal, depth) = horizonalDepthStartZero subActions
  putStrLn $ "Horzonal position is: " ++ show horizonal
  putStrLn $ "Depth is: " ++ show depth
  return $ horizonal * depth

parseInput :: String -> [SubAction]
parseInput str = map convertToTuple (lines str)

convertToTuple :: String -> SubAction
convertToTuple = convertToTuple2 . words

convertToTuple2 :: [String] -> SubAction
convertToTuple2 [xs, ys]
  | xs == "forward" = Forward $ read ys
  | xs == "up" = Up $ read ys
  | xs == "down" = Down $ read ys
convertToTuple2 _ = error "Wrong file format"

horizonalDepth :: (Int, Int) -> [SubAction] -> (Int, Int)
horizonalDepth (h, d) (Forward step : rest) = horizonalDepth (h + step, d) rest
horizonalDepth (h, d) (Down step : rest) = horizonalDepth (h, d + step) rest
horizonalDepth (h, d) (Up step : rest) = horizonalDepth (h, d - step) rest
horizonalDepth endPos [] = endPos

horizonalDepthStartZero :: [SubAction] -> (Int, Int)
horizonalDepthStartZero = horizonalDepth (0, 0)
