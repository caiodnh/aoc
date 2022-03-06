{-# LANGUAGE InstanceSigs #-}
module FunctionsVersion where

import WaitingRoom

-- Basic Types:
data RoomF = RoomF {func :: Position -> Seat, sizeF :: Size}

instance Eq RoomF where
  RoomF f sizef == RoomF g sizeg
    = sizef == sizeg && map f (allPos sizef) == map g (allPos sizef)

allPos :: Size -> [Position]
allPos (nRow, nCol) = [Position i j | i <- [0..nRow-1], j <- [0..nCol-1]]

instance WaitingRoom RoomF where

  (!)  :: RoomF -> Position -> Seat
  (!) room = func room

  (!?) :: RoomF -> Position -> Maybe Seat
  (!?) room pos@(Position i j)
    | 0 <= i && i < fst (sizeF room) && 0 <= j && j < snd (sizeF room)
      = Just $ func room pos
    | otherwise
      = Nothing 

  -- sizeRoom :: RoomF -> Size
  -- sizeRoom = sizeF

  newRoom :: Size -> (Position -> Seat) -> RoomF
  newRoom size f = RoomF f size

  readInput :: String -> (RoomF, Size)
  readInput input
    = (room, size)
    where  
      room = newRoom size mapping
      size = (nRows, nCols)
      input' = lines input
      nRows = length input'
      nCols = length $ head input'
      mapping (Position i j) = readChar $ (input' !! i) !! j

  numberOfOccupied :: RoomF -> Int
  numberOfOccupied room = length $ filter (== Occupied) $ func room <$> allPos (sizeF room)

  -- For Part 2:

newtype TableF = TableF (Position -> [Position])

instance TableOfAdjacency TableF where

  makeTable :: Size -> (Position -> [Position]) -> TableF
  makeTable (nRows, nCols) f
    = TableF f

  readTable :: TableF -> Position -> [Position]
  readTable (TableF f) pos
    = f pos