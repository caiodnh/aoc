module Circle where

-- Imports:

import Data.Char (digitToInt)

-- Cup type:

newtype Cup = Cup Int deriving (Eq, Ord)
type Current = Cup
type Removed = [Cup]

-- Global variables:

size :: Int
-- size = 10^6
size = 9

iterN :: Int
-- iterN = 10^7
iterN = 100

-- Instances: 

instance Enum Cup where
  toEnum n = Cup (n `mod` size)
  fromEnum (Cup 0) = size
  fromEnum (Cup n) = n

instance Show Cup where
  show = show . fromEnum

-- Circle class:
-- It represents a circle of cups, in which we can find the next cup "clockwise"

class Show a => Circle a where
  (!) :: a -> Cup -> Cup -- next cup "clockwise"
  (//) :: a -> [(Cup, Cup)] -> a -- update `a` by using `[(Cup, Cup)]`
  prepareInput :: [Int] -> (a, Cup) -- returns the the circle and the current cup

-- Functions:

readInput :: Circle a => String -> (a, Cup)
readInput input = prepareInput $ (digitToInt <$> input) ++ [10..size]

pick :: Circle a => (a, Current) -> (Removed, Current)
pick (cir, current) = fmap head $ splitAt 3 $ iterate (cir !) (cir ! current)

destination :: Current -> Removed -> Cup
destination oldCur removed
  = let dest = pred oldCur in
    if dest `elem` removed then destination dest removed
    else dest

updateCircle :: Circle a => (a, Current) -> (a, Current)
updateCircle (oldCir, oldCur) = (newCir, newCur)
  where
    newCir = oldCir // [(oldCur, newCur), (dest, head removed), (last removed, oldCir ! dest)] 
      -- ++ zip removed (tail removed))
    (removed, newCur) = pick (oldCir, oldCur)
    dest = destination oldCur removed

multiplyStars :: Circle a => a -> Int
multiplyStars circ
  = m * n
  where
    m = fromEnum (circ ! Cup 1)
    n = fromEnum (circ ! (circ ! Cup 1))