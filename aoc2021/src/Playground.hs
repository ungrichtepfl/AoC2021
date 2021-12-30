module Playground where

import Debug.Trace

data Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday deriving (Show)

data Shape
  = Circle
      { radius :: Int
      }
  | Rectangle
      { sideA :: Int,
        sideB :: Int
      }
  deriving (Show)

radius' :: Shape -> Int
radius' (Circle r) ={-hi-} r {-\hi-}
radius' _ = error "No radius."

area :: Shape -> Int
area (Circle r) | trace ("area circle " ++ show r) False = undefined
area (Circle r) = 3 * r * r
area (Rectangle a b) = a * b

sideA' :: Shape -> Int
sideA' (Rectangle a _) = a

giveFirst :: (Num a) => [a] -> a
giveFirst (x : y : rest) = x * y

map' :: (Show a, Show b) => (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : rest) = f x : map' f rest
