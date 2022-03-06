module Main where

import Data.Char

data Policy = Policy {minReap :: Int, maxReap :: Int, char :: Char}

type Password = String

readNumber :: String -> (Int, String)
readNumber str = (n, str')
  where
    (x, str') = span isDigit str
    n = read x

readLine :: String -> (Policy, Password)
readLine str = (pol, pword)
  where
    (n1, str1) = readNumber str
    str2 = tail str1
    (n2, str3) = readNumber str2
    str4 = tail str3
    c = head str4
    pword = drop 3 str4
    pol = Policy {minReap = n1, maxReap = n2, char = c}

readInput :: String -> [(Policy, Password)]
readInput = map readLine . lines

checkLine :: (Policy, [Char]) -> Bool
checkLine (pol, pword) = minReap pol <= n && n <= maxReap pol
  where
    n = length $ filter (\c -> c == char pol) pword

solve1 :: String -> Int
solve1 = length . filter id . map checkLine . readInput

checkLine2 :: (Policy, [Char]) -> Bool
checkLine2 (pol, pword) = (c == c' && c /= c'') || (c /= c' && c == c'')
  where
    c = char pol
    c' = pword !! (minReap pol - 1)
    c'' = pword !! (maxReap pol - 1)

solve2 :: String -> Int
solve2 = length . filter id . map checkLine2 . readInput

main :: IO ()
main = do
  s <- readFile "inputs/actual.txt"
  print $ solve2 s
