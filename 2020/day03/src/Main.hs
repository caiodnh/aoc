module Main where

import Data.Monoid

data Space = Open | Tree deriving Eq
type Chart = [[Space]]

(!) :: Chart -> (Int, Int) -> Space
chart ! (m, n) = (chart !! m) !! n

readInput :: String -> Chart
readInput = map (cycle . map translate) . lines
  where
    translate '.' = Open
    translate '#' = Tree

path :: Int -> Chart -> [Space]
path rightShift chart = list
  where
    (list, n) = foldl f ([], 0) chart
    f :: ([Space], Int) -> [Space] -> ([Space], Int)
    f (soFar, n) now = (soFar ++ [now !! n], n + rightShift)

solve1 :: String -> Int 
solve1 = length . filter (== Tree) . path 3 . readInput

numberOfTrees :: Int -> Chart -> Int
numberOfTrees n = length . filter (== Tree) . path n

removeEven :: Chart -> Chart
removeEven [] = []
removeEven [x] = [x]
removeEven (x1:x2:xs) = x1 : removeEven xs

main :: IO ()
main = do
  input <- readInput <$> readFile "input/actual.txt"
  let n1 = numberOfTrees 1 input
  let n2 = numberOfTrees 3 input
  let n3 = numberOfTrees 5 input
  let n4 = numberOfTrees 7 input
  let n5 = numberOfTrees 1 $ removeEven input
  print $ n1 * n2 * n3 * n4 * n5
