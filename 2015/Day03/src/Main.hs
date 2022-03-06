module Main where

import PartA (parseInput, solveA)
import PartB (solveB)

solve x = case x of
  'a' -> solveA
  'b' -> solveB
  _   -> error "Entry can only be 'a' or 'b'."

run :: Char -> IO ()
run x = readFile "input.txt" >>= print . solve x . parseInput

main :: IO ()
main = run 'a'
