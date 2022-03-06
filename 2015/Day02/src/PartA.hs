{-# LANGUAGE TupleSections #-}

module PartA where

import Control.Monad.State ( evalState, MonadState(state), State )
import Control.Applicative
import Data.List ( sort )

readSideStr :: String -> (String, String)
readSideStr ""       = ("", "")
readSideStr ('x':cs) = ("", cs)
readSideStr (c:cs)   = (c : beforeX, afterX)
  where
    (beforeX, afterX) = readSideStr cs

-- >>> readSideStr "12x30x2"
-- ("12","30x2")
-- >>> readSideStr "2x"
-- ("2","")

readSideStrState :: State String String
readSideStrState = state readSideStr

readSideNum :: State String Int -- State String -> (Int, String)
readSideNum = do
  str <- state readSideStr
  let num = read str
  return num

-- >>> runState readSideNum "12x30x2"
-- Variable not in scope: runState :: State String Int -> [Char] -> t
-- >>> runState readSideNum "30x2"
-- Variable not in scope: runState :: State String Int -> [Char] -> t
-- >>> runState readSideNum "2"
-- Variable not in scope: runState :: State String Int -> [Char] -> t

sortTriple :: Ord c => (c, c, c) -> (c, c, c)
sortTriple (x, y, z) = (a, b, c)
  where 
    [a, b, c] = sort [x, y, z]

readDims :: String -> (Int, Int, Int)
readDims = evalState $ do
  x <- readSideNum
  y <- readSideNum
  z <- readSideNum
  let [a, b, c] = sort [x, y, z]
  return (a, b, c)

-- >>> readDims "12x30x1"

readDimsApp :: String -> (Int, Int, Int)
readDimsApp = sortTriple . evalState stateAction
  where
    stateAction = liftA3' (,,) readSideNum
      where
        liftA3' f x = liftA3 f x x x

data Stream a =  a :~ Stream a

a = pure (+1) <*> ZipList [2, 3, 5]

-- return >=> f = f
-- g >=> return = g
-- (f >=> g) >=> h = f >=> (g >=> h)

-- >>> readDimsApp "12x30x2"
-- (2,12,30)

-- >>> readDims "12x30x2"
-- (2,12,30)

amountOfWrap :: (Int, Int, Int) -> Int 
amountOfWrap (a, b, c) = 3*a*b + 2*a*c + 2*b*c

parseInput :: String -> [(Int, Int, Int)]
parseInput = fmap readDims . lines

solveA :: [(Int, Int, Int)] -> Int 
solveA = sum . fmap amountOfWrap

a :: State String Int 
a = state f
  where
    f _ = (10, "a")

b :: State String (Int -> Int)
b = state g
  where 
    g _ = ((*3), "b")

-- >>> runState (bb <*> aa) "c"
-- Variable not in scope:
--   runState :: StateT String Identity Int -> [Char] -> t
