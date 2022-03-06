import Data.Char (digitToInt)
import Debug.Trace ( trace )

newtype Cup = Cup Int deriving (Eq)
type Circle = [Cup]
type Removed = [Cup]

instance Show Cup where
  show (Cup n) = show n

size :: Int
size = 10^6
iterN :: Int
iterN = 10^7

instance Enum Cup where
  toEnum n = Cup ((n - 1) `mod` size + 1)
  fromEnum (Cup n) = n

-- Functions:

-- The current is always the head
-- moveCurrentToEnd :: Circle -> Circle
-- moveCurrentToEnd cir = tail cir ++ head cir

readInput :: String -> Circle
readInput = map (Cup . digitToInt)


pick :: Circle -> (Removed, Circle)
pick = splitAt 3

destination :: Cup -> Removed -> Cup
destination cur removed
  = let dest = pred cur in
    if dest `elem` removed then destination dest removed
    else dest

splitAfter :: Eq a => [a] -> a -> ([a], [a])
splitAfter xs y = splitAfter' (id, xs)
  where 
    splitAfter' (appendPrev, after)
      = case after of
        []   -> error "splitting point not found" --(appendPrev [], after)
        x:xs -> if x == y then (appendPrev [x], xs)
                else splitAfter' (appendPrev . (x:), xs)

placeBack :: Removed -> Circle -> Cup -> Circle
placeBack removed cir dest
  = start ++ removed ++ end
  where
    (start, end) = splitAfter cir dest

update :: Circle -> Circle
update cir = placeBack removed cir'' dest
  where
    current = head cir
    cir' = tail cir ++ [current]
    (removed, cir'') = pick cir'
    dest = destination current removed

input :: Circle
input = readInput "389125467" ++ map Cup [10..size]

solve :: Int -> Circle -> Circle
-- solve n cir = iterate update cir !! n
solve n cir = iter (0, cir)
  where
    iter (i, c)
      | i == n = trace "Done!" c
      | otherwise = trace ("Iteration " ++ show i) iter (i+1, update c)

multiplyStars :: Circle -> Int
multiplyStars cir = n * m
  where 
    [Cup n, Cup m] = drop 2 after1
    after1 = snd pair ++ fst pair
    pair = splitAfter cir (Cup 1)

solve2 = multiplyStars . solve (10^7) 

main :: IO ()
main = print $ solve2 input