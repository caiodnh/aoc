import Data.List ( sort )
import Data.Maybe

-- Parsing input

parseInput :: String -> [String]
parseInput = lines

ioInput :: IO [String]
ioInput = parseInput <$> readFile "./day10.txt"

ioTestInput :: IO [String]
ioTestInput = parseInput <$> readFile "./day10.example.txt"

-- 

data Check = Correct | Incomplete Char | Corrupt Char deriving Show

closing :: Char -> Char 
closing c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _   -> error "Not an openning bracket."

checkLine :: String -> Check
checkLine [] = Correct
checkLine (a:as)
  = if a `notElem` "([{<" then Corrupt a
    else findClosing [a] as
    where
      findClosing []     bs     = checkLine bs
      findClosing (a:as) []     = Incomplete a
      findClosing (a:as) (b:bs)
        | b `elem` ")]}>" = if b == closing a
                            then findClosing as bs
                            else Corrupt b
        | otherwise       = findClosing (b:a:as) bs

pointCorrupt :: Check -> Int
pointCorrupt check = case check of
  Corrupt ')' -> 3
  Corrupt ']' -> 57
  Corrupt '}' -> 1197
  Corrupt '>' -> 25137
  _           -> 0

corruptPoints :: [Check] -> Int
corruptPoints = sum . fmap pointCorrupt

solve1 :: [String] -> Int
solve1 = corruptPoints . fmap checkLine

solution1 :: IO Int
solution1 = solve1 <$> ioInput

-- Part 2:

pointIncomplete bracket = case bracket of
  '(' -> 1
  '[' -> 2
  '{' -> 3
  '<' -> 4
  _   -> error "Not an oppening bracket." 

complete :: String -> Maybe [Int]
complete = aux []
  where
    aux points line = case checkLine line of
      Incomplete x -> aux (pointIncomplete x : points) (line ++ [closing x])
      Correct      -> Just points
      Corrupt _    -> Nothing

addPoints :: Num p => [p] -> p
addPoints [] = 0
addPoints (p:ps) = p + 5 * addPoints ps

findMiddle :: [Int] -> Int
findMiddle list = sort list !! (length list `div` 2)

solve2 :: [String] -> Int
solve2 = findMiddle . fmap addPoints . mapMaybe complete

solution2 = solve2 <$> ioInput