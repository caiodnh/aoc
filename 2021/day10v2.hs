import Data.List ( sort )
import Data.Maybe ( mapMaybe )

-- Parsing input

parseInput :: String -> [String]
parseInput = lines

ioInput :: IO [String]
ioInput = parseInput <$> readFile "./day10.txt"

ioTestInput :: IO [String]
ioTestInput = parseInput <$> readFile "./day10.example.txt"

-- 

type Bracket         = Char
type OpenningBracket = Char
type ClosingBracket  = Char

data Check = Correct
           | Incomplete [ClosingBracket]
           | Corrupt ClosingBracket
           deriving Show

close :: OpenningBracket -> ClosingBracket 
close c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _   -> error "Not an openning bracket."

openBracks :: [OpenningBracket]
openBracks = "([{<"

checkLine :: [Bracket] -> Check
checkLine = findClosing []
  where
    findClosing :: [ClosingBracket] -> [Bracket] -> Check
    findClosing [] []       = Correct
    findClosing closeds []  = Incomplete closeds
    findClosing closeds (b:bs)
      | b `elem` openBracks = findClosing (close b : closeds) bs
      | null closeds        = Corrupt b
      | head closeds == b   = findClosing (tail closeds) bs
      | otherwise           = Corrupt b

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

pointIncomplete ::ClosingBracket -> Int
pointIncomplete bracket = case bracket of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _   -> error "Not a closing bracket." 

totalPoints :: [ClosingBracket] -> Int
totalPoints = foldl f 0
  where
    f acum b = acum * 5 + pointIncomplete b

remainingToComplete :: Check -> Maybe [ClosingBracket]
remainingToComplete check
  = case check of
  Incomplete remaining -> Just remaining
  _                    -> Nothing

findMiddle :: [Int] -> Int
findMiddle list = sort list !! (length list `div` 2)

solve2 :: [String] -> Int
solve2 = findMiddle . fmap totalPoints. mapMaybe (remainingToComplete . checkLine)

solution2 :: IO Int
solution2 = solve2 <$> ioInput