module Main where

import Data.Maybe

readInput :: String -> [Int]
readInput = map read . lines

check :: Int -> Int -> Maybe (Int, Int)
check a b
  = if a + b == 2020 then Just (a,b) else Nothing 

findPairs :: [Int] -> [(Int,Int)]
findPairs input
  = case input of
    [] -> []
    (n:ns) -> mapMaybe (check n) ns ++ findPairs ns

solve2 :: [Int] -> Int
solve2 input = head [a*b*c | a <- input, b <- input, c <- input, a + b + c == 2020]

main1 :: IO ()
main1 = readFile "src/actual.txt" >>= print . map (uncurry (*)) . findPairs . readInput

main :: IO ()
main = readFile "src/actual.txt" >>= print . solve2 . readInput