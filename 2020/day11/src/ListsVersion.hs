{-# LANGUAGE InstanceSigs #-}
module ListsVersion where

import WaitingRoom

-- Basic Types:
data RoomL = RoomL {grid :: [[Seat]], sizeL :: Size}

instance Eq RoomL where
  room == room' = grid room == grid room'

instance WaitingRoom RoomL where

  (!)  :: RoomL -> Position -> Seat
  (!) room (Position i j) = (grid room !! i) !! j

  (!?) :: RoomL -> Position -> Maybe Seat
  (!?) room pos@(Position i j)
    | 0 <= i && i < fst (sizeL room) && 0 <= j && j < snd (sizeL room)
      = Just $ room ! pos
    | otherwise
      = Nothing 

  -- sizeRoom :: RoomL -> Size
  -- sizeRoom (RoomL matrix)= (length matrix, length $ head matrix)

  newRoom :: Size -> (Position -> Seat) -> RoomL
  newRoom (nRows, nCols) f = RoomL {
      grid  = [ [ f (Position i j) | j <- [0.. nCols - 1] ] | i <- [0.. nRows - 1] ],
      sizeL = (nRows, nCols)
    }

  readInput :: String -> (RoomL, Size)
  readInput input
    = (RoomL matrix size, size)
    where
      input' = lines input
      nRows = length input'
      nCols = length $ head input'
      size = (nRows, nCols)
      matrix = map (map readChar) input'

  numberOfOccupied :: RoomL -> Int
  numberOfOccupied room = sum $ map f (grid room)
    where f = foldr (\seat -> if seat == Occupied then (1+) else id) 0

  -- For Part 2:

newtype TableL = TableL [[[Position]]]

instance TableOfAdjacency TableL where

  makeTable :: Size -> (Position -> [Position]) -> TableL
  makeTable (nRows, nCols) f
    = TableL $ [ [ f (Position i j) | j <- [0.. nCols - 1] ] | i <- [0.. nRows - 1] ]

  readTable :: TableL -> Position -> [Position]
  readTable (TableL matrix) (Position i j)
    = (matrix !! i ) !! j