{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications #-}

module WaitingRoom where

import Data.Maybe ( mapMaybe )
import Debug.Trace ( trace )

-- Types

data Seat = Floor | Free | Occupied deriving (Eq, Show)

data Position = Position Int Int

newtype Direction = D (Int, Int)

type Size = (Int, Int)

-- Class

class (Eq a) => WaitingRoom a where
  (!)  :: a -> Position -> Seat
  (!?) :: a -> Position -> Maybe Seat
  -- sizeRoom :: a -> Size
  newRoom :: Size -> (Position -> Seat) -> a
  readInput :: String -> (a, Size)
  numberOfOccupied :: a -> Int

-- Functions for both parts:

readChar :: Char -> Seat
readChar 'L' = Free
readChar '#' = Occupied
readChar '.' = Floor
readChar  _  = error "character not allowed"

(+>) :: Position -> Direction -> Position
(Position i j) +> (D (i', j')) = Position (i + i') (j + j')

letItStabilize :: WaitingRoom a => Int -> (a -> a) -> a -> a
letItStabilize n f room
  = trace ("Iteration number " ++ show n) $
    let room' = f room in
    if room == room' then trace "Done!" room else letItStabilize (n+1) f room'

-- Functions for Part 1

aroundPoint :: WaitingRoom a => a -> Position -> [Maybe Seat]
aroundPoint room pos  
  = map ((room !?) . (pos +>)) aroundVectors
  where
    aroundVectors 
      = [D (-1, 1), D (0, 1), D (1, 1),
         D (-1, 0),           D (1, 0),
         D (-1,-1), D (0,-1), D (1,-1)]

updateSeat :: WaitingRoom a => a -> Position -> Seat
updateSeat room pos
  = case room ! pos of 
    Floor    -> Floor
    Free     -> if noOccupiedAround then Occupied else Free
    Occupied -> if tooCrowded then Free else Occupied
    where 
      noOccupiedAround = Just Occupied `notElem` aroundOld
      tooCrowded = length (filter (== Just Occupied) aroundOld) >= 4
      aroundOld = aroundPoint room pos

updateRoomWithSize :: WaitingRoom a => Size -> a -> a
-- Size is given to avoid compute it many times
updateRoomWithSize size oldRoom
  = newRoom size (updateSeat oldRoom)

solve1 :: WaitingRoom a => (a, Size) -> Int
solve1 (room, fixedSize)
  = numberOfOccupied $ letItStabilize 1 updateRoom room -- 1 added for the trace version
  where
    updateRoom = updateRoomWithSize fixedSize

-- Part 2:

class TableOfAdjacency b where
  makeTable :: Size -> (Position -> [Position]) -> b
  readTable :: b -> Position -> [Position]

findClosest :: WaitingRoom a => a -> Position -> Direction -> Maybe Position
findClosest room pos dir
  = let pos' = pos +> dir in
    case room !? pos' of
      Nothing -> Nothing
      Just Floor -> findClosest room pos' dir
      _ -> Just pos'

adjacentPositions :: WaitingRoom a => a -> Position -> [Position]
adjacentPositions room pos
  = mapMaybe (findClosest room pos) aroundVectors
  where
    aroundVectors :: [Direction]
    aroundVectors
      = [D (-1, 1), D (0, 1), D (1, 1),
         D (-1, 0),           D (1, 0),
         D (-1,-1), D (0,-1), D (1,-1)]

aroundPoint2 :: (TableOfAdjacency b, WaitingRoom a) => b -> a -> Position -> [Seat]
aroundPoint2 table room pos
  = (room !) <$> readTable table pos

updateSeat2 :: (TableOfAdjacency b, WaitingRoom a) => b -> a -> Position -> Seat
updateSeat2 table room pos
  = case room ! pos of 
    Floor    -> Floor
    Free     -> if noOccupiedAround then Occupied else Free
    Occupied -> if tooCrowded then Free else Occupied
    where 
      noOccupiedAround = Occupied `notElem` aroundOld
      tooCrowded = length (filter (== Occupied) aroundOld) >= 5
      aroundOld = aroundPoint2 table room pos

updateRoomWithSize2 :: (TableOfAdjacency b, WaitingRoom a) => Size -> b -> a -> a
-- Size is given to avoid compute it many times
updateRoomWithSize2 size table oldRoom
  = newRoom size (updateSeat2 table oldRoom)

solve2 :: forall b a. (TableOfAdjacency b, WaitingRoom a) => (a, Size) -> Int
solve2 (room, fixedSize)
  = numberOfOccupied $ letItStabilize 1 updateRoom room -- 1 added for the trace version
  where
    updateRoom = updateRoomWithSize2 fixedSize fixedTable
    fixedTable = makeTable fixedSize (adjacentPositions room) :: b

-- Merging solves
type Part = Int

solve :: forall b a. (TableOfAdjacency b, WaitingRoom a) => Part -> (a, Size) -> Int
solve 1 = solve1
solve 2 = solve2 @b

