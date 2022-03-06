import Data.Map ( (!), Map )
import qualified Data.Map as Map

import Data.List.Split ( splitOn )
import Data.Maybe (catMaybes)

import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set

import Data.List

type Code = Set Char

numberOfSegments :: Map Int Int
numberOfSegments = Map.fromList $ zip [0..9] [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]

possibleDigits :: Map Int [Int]
possibleDigits = Map.fromList [(2, [1]),
                           (3, [7]),
                           (4, [4]),
                           (5, [2,3,5]),
                           (6, [0,6,9]),
                           (7, [8])]

-- codeToDigit :: Map Code Int
-- codeToDigit = Map.fromList $ zip ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"] [0..9]

ioInput :: IO [([Code], [Code])]
ioInput = parseFile <$> readFile "./day08.txt"
  where
    parseFile :: String -> [([Code], [Code])]
    parseFile = fmap parseLine . lines
    parseLine = fmap tail . splitAt 10 . fmap Set.fromList . splitOn " "

ioResults :: IO [[Code]]
ioResults = fmap snd <$> ioInput

solveEasyCode :: Code -> Maybe Int 
solveEasyCode code
  = singletonToMaybe $ possibleDigits ! length code
    where
      singletonToMaybe [a] = Just a
      singletonToMaybe _   = Nothing

solveEasyLine :: [Code] -> [Int]
solveEasyLine = catMaybes . fmap solveEasyCode

solve1 :: [[Code]] -> Int
solve1 = sum . fmap (length . solveEasyLine)

solution1 :: IO Int
solution1 = solve1 <$> ioResults

-- Part 2

firstStep :: [Code] -> (Code, Code, Code, Code, [Code], [Code])
firstStep codes = aux sortedCodes
  where
    sortedCodes = sortOn Set.size codes
    
    aux [s2, s3, s4, s51, s52, s53, s61, s62, s63, s7]
      = (s2, s3, s4, s7, [s51, s52, s53], [s61, s62, s63])
    aux _ = error "Not 10 codes" 

secondStep :: (Code, Code, Code, Code, [Code], [Code]) -> Map Code Int
secondStep (c1, c7, c4, c8, size5, size6)
  = ans
  where
    ([c3], size5') = partition (c1 `isSubsetOf`) size5
    ([c9], size6') = partition (c3 `isSubsetOf`) size6
    ([c5], [c2])   = partition (`isSubsetOf` c9) size5'
    ([c0], [c6])   = partition (c1 `isSubsetOf`) size6'
    ans = Map.fromList $ zip [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9] [0..9]

thirdStep :: Map Code Int -> [Code] -> [Int]
thirdStep translation = fmap (translation !)

fourthStep :: [Int] -> Int
fourthStep = aux . reverse
aux [] = 0
aux (d:ds) = d + 10 * aux ds

solve2 :: [([Code], [Code])] -> Int 
solve2 = sum . fmap solveLine
  where
    solveLine (allCodes, output)
      = fourthStep $ (thirdStep . secondStep . firstStep $ allCodes) output

solution2 :: IO Int
solution2 = solve2 <$> ioInput