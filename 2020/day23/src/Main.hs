{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}

module Main where

import Time ( time )
import Circle
    ( Circle, Current, iterN, readInput, updateCircle, multiplyStars )
import CircleVector ( CircleV )
import CircleMap ( CircleM )
import CircleVectorMut ( CircleVM ) 
import qualified CircleMut as Mut

example :: [Char]
example = "389125467"
actual :: [Char]
actual = "398254716"

input :: Circle a => (a, Current)
input = readInput example

solve :: Circle a => Int -> (a, Current) -> (a, Current)
-- solve n cir = iterate update cir !! n
solve n (cir, cur) = iter (0, (cir, cur))
  where
    iter (i, (cir, cur))
      | i == n = (cir, cur)
      | otherwise = iter (i+1, updateCircle (cir, cur))

solve1 :: Circle a => Int -> (a, Current)
solve1 n = solve n input

solve2 :: Circle a => (a, Current) -> Int
solve2 = multiplyStars . fst . solve iterN

main :: IO ()
main = time $ print $ solve2 $ readInput @CircleM example
