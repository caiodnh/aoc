{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

module CircleMut where

-- Imports:

import Data.Char (digitToInt)

-- Cup type:

newtype Cup = Cup Int deriving (Eq, Ord)

-- Global variables:

size :: Int
size = 10^6
-- size = 9

iterN :: Int
iterN = 10^7
-- iterN = 100

-- Instances: 

instance Enum Cup where
  toEnum n = Cup (n `mod` size)
  fromEnum (Cup 0) = size
  fromEnum (Cup n) = n

instance Show Cup where
  show = show . fromEnum

-- CircleIO class:
-- It represents a circle of cups, in which we can find the next cup "clockwise"

class CircleIO a where
  (!) :: a -> Cup -> IO Cup -- next cup "clockwise"
  (//) :: a -> [(Cup, Cup)] -> IO () -- update `a` by using `[(Cup, Cup)]`
  prepareInput :: [Int] -> IO (a, Cup) -- returns the the CircleIO and the current cup
  printCircleIO :: a -> IO ()

-- Functions:

readInput :: CircleIO a => String -> IO (a, Cup)
readInput input = prepareInput $ (digitToInt <$> input) ++ [10..size]

pick :: CircleIO a => (a, Cup) -> IO ([Cup], Cup)
pick (cir, current) = do
  list <- sequence $ iterate ((cir !) =<<) (cir ! current)
  return $ head <$> splitAt 3 list

-- elemIO :: Cup -> [Cup] -> IO Bool 
-- elemIO cup
--   = foldr f (return False)
--   where
--     f x = (>>= \y -> return ( y || cup == x))


destination :: Cup -> [Cup] -> Cup
destination oldCur removed
  = let dest = pred oldCur in
    if dest `elem` removed
      then destination dest removed
    else
      dest

updateCircleIO :: CircleIO a => Int -> a -> Cup -> IO Cup
updateCircleIO i oldCir oldCur =
  do
    print i
    (removed, newCur) <- pick (oldCir, oldCur)
    let dest = destination oldCur removed
    afterDest <- oldCir ! dest
    oldCir // [(oldCur, newCur), (dest, head removed), (last removed, afterDest)]
    return newCur

multiplyStars :: CircleIO a => a -> IO Int
multiplyStars circ
  = do
      m <- fromEnum <$> (circ ! Cup 1)
      n <- fromEnum <$> ((circ ! Cup 1) >>= (circ !))
      return $ m * n

-- Solve

solve :: forall a. CircleIO a => String -> Int -> IO ()
solve input n = do
  (circle, cur0) <- readInput @a input
  -- let iter m current
  --       = if m == 0
  --           then return current
  --         else
  --           updateCircleIO (circle, current) >>= iter (m-1)
  -- curN <- iter n cur0
  ------
  let f i current = current >>= updateCircleIO i circle
  curN <- foldr f (return cur0) [1..n]
  printCircleIO circle
  print curN