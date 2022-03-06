import Data.List

type Fish = Int

day :: [Fish] -> [Fish]
day [] = []
day (f:fs)
  | f == 0    = 8 : 6 : day fs
  | otherwise = (f-1): day fs

daysAfter :: Int -> [Fish] -> [Fish]
n `daysAfter` fs = last $ take (n + 1) $ iterate day fs

solve1 :: [Fish] -> Int 
solve1 fs = length $ 80 `daysAfter` fs

input :: [Int]
input = [1,2,1,3,2,1,1,5,1,4,1,2,1,4,3,3,5,1,1,3,5,3,4,5,5,4,3,1,1,4,3,1,5,2,5,2,4,1,1,1,1,1,1,1,4,1,4,4,4,1,4,4,1,4,2,1,1,1,1,3,5,4,3,3,5,4,1,3,1,1,2,1,1,1,4,1,2,5,2,3,1,1,1,2,1,5,1,1,1,4,4,4,1,5,1,2,3,2,2,2,1,1,4,3,1,4,4,2,1,1,5,1,1,1,3,1,2,1,1,1,1,4,5,5,2,3,4,2,1,1,1,2,1,1,5,5,3,5,4,3,1,3,1,1,5,1,1,4,2,1,3,1,1,4,3,1,5,1,1,3,4,2,2,1,1,2,1,1,2,1,3,2,3,1,4,5,1,1,4,3,3,1,1,2,2,1,5,2,1,3,4,5,4,5,5,4,3,1,5,1,1,1,4,4,3,2,5,2,1,4,3,5,1,3,5,1,3,3,1,1,1,2,5,3,1,1,3,1,1,1,2,1,5,1,5,1,3,1,1,5,4,3,3,2,2,1,1,3,4,1,1,1,1,4,1,3,1,5,1,1,3,1,1,1,1,2,2,4,4,4,1,2,5,5,2,2,4,1,1,4,2,1,1,5,1,5,3,5,4,5,3,1,1,1,2,3,1,2,1,1]

solution1 :: Int
solution1 = solve1 input

-- Part 2

-- Let us consider the vector in R^9 with entries given by the number of fish with that delay

countFish = aux 0
  where 
    aux 9 fs = []
    aux n fs = length (filter (== n) fs) : aux (n+1) fs

dot :: [Int] -> [Int] -> Int
dot [] _ = 0
dot _ [] = 0
dot (v:vs) (w:ws) = v*w + dot vs ws

times :: [[Int]] -> [[Int]] -> [[Int]]
times m1 m2 = auxTimes m1 (transpose m2)
  where
    auxTimes [] _ = []
    auxTimes (r:rs) cs =  fmap (r `dot`) cs: auxTimes rs cs

to2toN :: [[Int]] -> Int -> [[Int]]
to2toN m 0 = m
to2toN m n = let m' = m `to2toN` (n - 1) in m' `times` m'

matrix = [[0, 1, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 1, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 1, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 1, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 1, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 1, 0],
          [0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0]]

allDays logDays = matrix `to2toN` logDays

initVector :: [Fish] -> [[Int]]
initVector fs = return <$> countFish fs

resultVector :: [Fish] -> Int -> [[Int]]
resultVector fs logDays = allDays logDays `times` initVector fs

solve2 :: [Fish] -> Int
solve2 fs = sum $ concat $ resultVector fs 8

solution2 = solve2 input