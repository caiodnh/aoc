{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

import Time (time)

import WaitingRoom
    ( Size, TableOfAdjacency, WaitingRoom(readInput), solve )

import FunctionsVersion ( RoomF, TableF )
import ListsVersion ( RoomL, TableL )
import VectorsVersion ( RoomVec, TableVec )

-- Types:

type Part  = Int
type Input = String

-- Inputs:

example :: Input
example = "inputs/example.txt"

actual :: Input
actual = "inputs/actual.txt"

-- Work around to "pass types as arguments"

roomV :: RoomVec
roomF :: RoomF
roomL :: RoomL
roomV = undefined
roomF = undefined
roomL = undefined

tableV :: TableVec
tableF :: TableF
tableL :: TableL
tableV = undefined
tableF = undefined
tableL = undefined

-- asTypeOf :: a -> a -> a
-- asTypeOf x y = x

-- room = readInput `asTypeOf` roomV

-- Solving part

readAndSolve :: forall r t. (WaitingRoom r, TableOfAdjacency t) => Part -> Input -> IO()
readAndSolve part input =
  do
    s <- readFile input
    let x = readInput s :: (r, Size)
    print $ solve @t part x

timedReadAndSolve :: forall r t. (WaitingRoom r, TableOfAdjacency t) => Part -> Input -> IO ()
timedReadAndSolve part input =
  do
    putStrLn "Starting..."
    time $ readAndSolve @r @t part input
    putStrLn "Done."

main :: IO ()
main = timedReadAndSolve @RoomVec @TableVec 2 actual