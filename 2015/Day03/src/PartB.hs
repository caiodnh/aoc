module PartB where

import qualified PartA as A
import PartA (Pos)
import Data.Set
import Data.Monoid

tripB :: Pos -> Pos -> Set Pos -> [Pos] -> Set Pos
tripB current0 current1 previous commands
  = case commands of
    []   -> insert current1 $ insert current0 previous
    [c]  -> insert current0 previous
    c0 : c1 : cs -> tripB (c0 <> current0) (c1 <> current1) (insert current1 $ insert current0 previous) cs

visitedB :: [Pos] -> Set Pos
visitedB = tripB (Sum 0, Sum 0) (Sum 0, Sum 0) empty

solveB :: [Pos] -> Int
solveB = size . visitedB