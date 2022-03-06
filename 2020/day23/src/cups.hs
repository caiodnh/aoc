{-# LANGUAGE FlexibleInstances #-}

import Data.Char (digitToInt)
import Data.Array
import Data.Maybe (mapMaybe)
import Data.List ( unfoldr )
import Debug.Trace ( trace )

-- Types:

newtype Cup = Cup {label :: Int} deriving (Eq)
newtype Position = Pos {_toInt :: Int} deriving (Eq, Show, Ord, Ix)
type Circle = Array Position

type Current = Position
type Picked = [Cup]

-- Instances:

lowerBound :: Position
upperBound :: Position
lowerBound = Pos 1
upperBound = Pos size
size :: Int
size = 9
-- size = 10^6

instance Show Cup where
  show = show.label

-- instance Show a => Show (Circle a) where
--   show = concatMap show . elems

toString :: Show a => Circle a -> String
toString = concatMap show . elems

instance Enum Cup where
  toEnum n = Cup ((n - 1) `mod` size + 1)
  fromEnum = label

instance Enum Position where
  toEnum n = Pos ((n - 1) `mod` size + 1)
  fromEnum = _toInt

-- Input

example :: [Char]
example = "389125467"

-- Functions

readInput :: String -> Circle Cup
readInput = listArray (Pos 1, Pos size) . map (Cup . digitToInt)

-- Not modifying the Circle:

pickCups :: Circle Cup -> Position -> [Cup]
pickCups cir currentPos
  = map (cir !) removedPositions
  where
    removedPositions = take 3 $ iterate succ (succ currentPos)
    -- removed = map (cir !) removedPositions
    -- newCirc = fmap Just cir // zip removedPositions (repeat Nothing)

destination :: Cup -> [Cup] -> Cup
destination currentValue picked
  = destCup currentValue
  where
    destCup cup = let cup' = pred cup in 
      if cup' `elem` picked
      then destCup cup'
      else cup'

findPosition :: Eq a => Circle a -> a -> Position
findPosition cir cup = iter lowerBound
  where
    iter pos
      | cir ! pos == cup = pos
      | otherwise  = iter $ succ pos

-- Removing the sublist from the circle

placeBack :: Circle Cup -> Position -> Position -> [Cup] -> Circle Cup
placeBack cir currentPos destinationPos removed
  = cir // zip changedPositions newValues
  where
    start = succ destinationPos
    stop = succ currentPos
    changedPositions = iterate succ start 
    newValues = removed ++ continuation
    continuation = unfoldr f start
    f position
      | position == stop = Nothing 
      | otherwise = Just (cir ! position, succ position)

newCurrent :: Circle Cup -> Circle Cup -> Position -> Position
-- newCurrent circOld circNew pos = succ $ findPosition circNew (circOld ! pos)

newCurrent _ _ = succ . succ . succ

update :: (Circle Cup, Position) -> (Circle Cup, Position)
update (circ, current) = (circ', current')
  where
    circ' = placeBack circ current destPos removed
    destPos = findPosition circ destCup
    removed = pickCups circ current
    destCup = destination (circ ! current) removed
    current' = newCurrent circ circ' current
    message = "Circle: " ++ toString circ ++ "\nCurrent Pos: " ++ show current

solve1 input n = toString $ fst $ (!! n) $ iterate update (circ, Pos 1)
  where
    circ = readInput input

-- destination :: Cup -> Selection -> Cup
-- destination cup selection
--   = let cup' = pred cup in
--     if cup' `elem` selection then destination cup' selection
--     else cup'

-- makeLa :: Cup ->

-- insert :: (Selection, Circle) -> Cup -> Circle
-- insert (sel, c:cs) dest
--   | c == dest = (c:) $ sel ++ cs
--   | otherwise = (c:) $ insert (sel, cs) dest

-- newCurrent :: 