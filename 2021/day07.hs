import Data.List.Split ( splitOn )
import Data.List ( sort )

ioInput :: IO [Int]
ioInput = sort . fmap read . splitOn "," <$> readFile "day07.txt"

median :: [Int] -> Int
median sortedList = let pos = length sortedList `quot` 2
                    in sortedList !! pos

totalCost :: [Int] -> Int 
totalCost = do
  m <- median
  sum . fmap (\x -> abs (x - m))

solution1 :: IO Int
solution1 = totalCost <$> ioInput

--

cost :: Int -> Int -> Int
cost x xi = let a = abs (x - xi) in (a + 1) * a

totalCostGivenX :: Int -> [Int] -> Int
totalCostGivenX x = sum . fmap (cost x)

bestCost :: [Int] -> Int
bestCost xs = minimum costs `div` 2
  where
    costs :: [Int]
    costs = do
      x <- [head xs..last xs]
      return $ totalCostGivenX x xs

solution2 = bestCost <$> ioInput