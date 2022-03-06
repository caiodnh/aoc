module PartA where

import Data.Monoid
import Data.Set

type Pos = (Sum Int, Sum Int)

readCommand :: Char -> Pos
readCommand x = case x of
  '^' -> (Sum 0, Sum 1)
  '>' -> (Sum 1, Sum 0)
  'v' -> (Sum 0, Sum (-1))
  '<' -> (Sum (-1), Sum 0)
  _   -> undefined 

parseInput :: String -> [Pos]
parseInput = fmap readCommand

trip :: Pos -> Set Pos -> [Pos] -> Set Pos
trip current previous commands
  = case commands of
    []   -> previous
    c:cs -> trip (c <> current) (insert current previous) cs

visited :: [Pos] -> Set Pos
visited = trip (Sum 0, Sum 0) empty

solveA :: [Pos] -> Int
solveA = size . visited