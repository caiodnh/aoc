import Control.Monad.State

count :: [Int] -> Int
count input = fst $ foldl f (0, head input) (tail input)
  where
    f (count, x) y = if x < y then (count + 1, y) else (count, y)

parseInput :: String -> [Int]
parseInput = fmap read . lines

solve1 :: IO Int
solve1 = count . parseInput <$> readFile "./day01.txt"

-- State, v1

compareWithState :: Int -> State (Int, Int) ()
compareWithState new = do
  (past, count) <- get
  if past < new
    then put (new, count + 1)
    else put (new, count)

count' :: [Int] -> Int
count' input = snd $ execState (mapM compareWithState (tail input)) (head input, 0)

-- State, v2

compareWithState2 :: Int -> State Int Int
compareWithState2 new = do
  past <- get 
  put new
  if past < new
    then return 1
    else return 0

count'' :: [Int] -> Int
count'' input = sum $ evalState (sequence $ fmap compareWithState2 (tail input)) (head input)

-- Part 2

first3 :: [Int] -> [Int]
first3 (x0:x1:x2:xs) = [x0, x1, x2]
first3 _             = error "error1"

tails :: [a] -> [[a]]
tails []    = []
tails [x]   = []
tails [x,y] = []
tails as = take 3 as: tails (tail as)

sums :: [Int] -> [Int]
sums = fmap sum . tails

solve2  :: IO Int
solve2 = count . sums . parseInput <$> readFile "./day01.txt"

solve2' :: IO Int
solve2' = count' . sums . parseInput <$> readFile "./day01.txt"

solve2'' :: IO Int
solve2'' = count'' . sums . parseInput <$> readFile "./day01.txt"