-- For the table of costs:
import Data.Array
import Data.List (transpose)
import Data.Char (digitToInt)

-- # Types

-- Basic Types:
type Vertex   = (Int, Int)
data Distance = Fin Int | Infinite deriving (Show, Eq, Ord)
type Costs = Array Vertex Distance

instance Semigroup Distance where
  (Fin x) <> (Fin y) = Fin $ x + y
  _       <> _       = Infinite

newtype TropMat = TropMat [[Distance]]

instance Semigroup TropMat where
  (TropMat m1) <> (TropMat m2)
    = TropMat $ aux m1 (transpose m2)
    where
      aux m1 m2T = do
        row <- m1
        let rowByCol = minimum . zipWith (<>) row
        return $ fmap rowByCol m2T

-- indentity (bot, top)
--   = let size = range (bot, top) in
--     foldl 

power :: TropMat -> Int -> TropMat
power matrix 1 = matrix
power matrix n
  = let (q, r) = quotRem n 2 in
    if r == 0
      then square (power matrix q)
      else square (power matrix q) <> matrix
      where
        square matrix = matrix <> matrix

-- -- parsing

parseEntry :: String -> Costs
parseEntry input = array
  where
    rawMatrix = lines input
    rows = length rawMatrix - 1
    cols = length (head rawMatrix) - 1
    preArray = Fin . digitToInt <$> concat rawMatrix
    array = listArray ((0,0), (rows,cols)) preArray

exInput :: IO Costs
exInput = parseEntry <$> readFile "./day15.ex.txt"

ioInput :: IO Costs
ioInput = parseEntry <$> readFile "./day15.input.txt"

-- part II

incRisk :: Distance -> Distance
incRisk Infinite = Infinite
incRisk (Fin 9) = Fin 1
incRisk (Fin n) = Fin $ n+1

parse2 :: String -> Costs
parse2 input = listArray ((0,0), (rows,cols)) preArray
  where
    rawMatrix = lines input
    rows = 5 * length rawMatrix - 1
    cols = 5 * length (head rawMatrix) - 1
    distMatrix = fmap (fmap (Fin . digitToInt) ) rawMatrix
    increasedToLeft
      = concat . take 5 . iterate (fmap incRisk) <$> distMatrix
    increasedDown = concat $ take 5 $ iterate (fmap (fmap incRisk)) increasedToLeft
    preArray = concat increasedDown

exInput2 :: IO Costs
exInput2 = parse2 <$> readFile "./day15.ex.txt"

ioInput2 :: IO Costs
ioInput2 = parse2 <$> readFile "./day15.input.txt"

-- 