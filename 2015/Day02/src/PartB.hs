module PartB where

import qualified PartA as A

amountOfRibbon :: (Int, Int, Int) -> Int 
amountOfRibbon (a, b, c) = 2*a + 2*b + a*b*c

solveB :: [(Int, Int, Int)] -> Int 
solveB = sum . fmap amountOfRibbon
