{-# LANGUAGE InstanceSigs #-}

module VectorsVersion where

import WaitingRoom
import qualified Data.Vector as V

-- Instance

newtype RoomVec = RoomVec (V.Vector (V.Vector Seat)) deriving (Eq)

instance WaitingRoom RoomVec where
  (!)  :: RoomVec -> Position -> Seat
  (!) (RoomVec vecOfVecs) (Position i j)
    = (vecOfVecs V.! i) V.! j

  (!?) :: RoomVec -> Position -> Maybe Seat
  (!?) (RoomVec vecOfVecs) (Position i j)
    -- = vecOfVecs V.!? i >>= (V.!? j)
    = case vecOfVecs V.!? i of
      Nothing -> Nothing
      Just vector -> vector V.!? j

  -- sizeRoom :: RoomVec -> Size
  -- sizeRoom (RoomVec vecOfVecs)
  --   = (V.length vecOfVecs, V.length $ V.head vecOfVecs)

  newRoom :: Size -> (Position -> Seat) -> RoomVec
  newRoom (nRows, nCols) roomMap
    = RoomVec $ V.generate nRows row
    where
      row :: Int -> V.Vector Seat
      row i = V.generate nCols (roomMap . Position i)

  readInput :: String -> (RoomVec, Size)
  readInput input = (RoomVec vecOfVecs, size)
    where
      vecOfVecs
        = let input' = lines input in
        V.fromList $ map (V.fromList . map readChar) input'
      size
        = (V.length vecOfVecs, V.length $ V.head vecOfVecs)

  numberOfOccupied :: RoomVec -> Int
  numberOfOccupied (RoomVec vecOfVecs)
    = V.sum $ fmap f vecOfVecs
    where
      f = V.foldr (\seat -> if seat == Occupied then (1+) else id) 0

-- For Part 2:

newtype TableVec = TableVec (V.Vector (V.Vector [Position])) 

instance TableOfAdjacency TableVec where

  makeTable :: Size -> (Position -> [Position]) -> TableVec
  makeTable (nRows, nCols) f
    = TableVec $ V.generate nRows row
    where
      row :: Int -> V.Vector [Position]
      row i = V.generate nCols (f . Position i)

  readTable :: TableVec -> Position -> [Position]
  readTable (TableVec matrix) (Position i j)
    = (matrix V.! i ) V.! j