module Day02
  ( day02Part1,
    day02Part2,
  )
where

data SubAction = Up Int | Down Int | Forward Int deriving (Show)

day02Part1 :: FilePath -> IO Int
day02Part1 filePath = do
  contents <- readFile filePath
  let subActions = parseInput contents
  let (horizontal, depth) = horizonalDepthStartZero subActions
  putStrLn $ "Horizontal position is: " ++ show horizontal
  putStrLn $ "Depth is: " ++ show depth
  return $ horizontal * depth

day02Part2 :: FilePath -> IO Int
day02Part2 filePath = do
  contents <- readFile filePath
  let subActions = parseInput contents
  let (horizontal, depth, aim) = horizonalDepthAimStartZero subActions
  putStrLn $ "Aim is: " ++ show aim
  putStrLn $ "Horizontal position is: " ++ show horizontal
  putStrLn $ "Depth is: " ++ show depth
  return $ horizontal * depth

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

horizonalDepthAimStartZero :: [SubAction] -> (Int, Int, Int)
horizonalDepthAimStartZero = horizonalDepthAim (0, 0, 0)

horizonalDepthAim :: (Int, Int, Int) -> [SubAction] -> (Int, Int, Int)
horizonalDepthAim (h, d, a) (Forward step : rest) = horizonalDepthAim (h + step, d + step * a, a) rest
horizonalDepthAim (h, d, a) (Down step : rest) = horizonalDepthAim (h, d, a + step) rest
horizonalDepthAim (h, d, a) (Up step : rest) = horizonalDepthAim (h, d, a - step) rest
horizonalDepthAim endPos [] = endPos
