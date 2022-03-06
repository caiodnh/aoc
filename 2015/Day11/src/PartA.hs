{-# LANGUAGE MonadComprehensions #-}

module PartA where

import Control.Monad.State

parseInput :: String -> String
parseInput = id

genNextCandidate :: String -> String  
genNextCandidate = reverse . genNextCandidate' . reverse
  where
    genNextCandidate' [] = "a"
    genNextCandidate' (c:cs)
      | c == 'z'  = 'a' : genNextCandidate' cs
      | otherwise = succ c : cs

-- Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
hasSequence :: String -> Bool 
hasSequence [] = False 
hasSequence [c0] = False 
hasSequence [c0, c1] = False 
hasSequence (c0:c1:c2:cs)
  | c1 == succ c0 && c2 == succ c1 = True 
  | otherwise                      = hasSequence (c1:c2:cs)

-- Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
onlyValidChars :: String -> Bool 
onlyValidChars = not . any (`elem` "iol")
-- onlyValidChars [] = True
-- onlyValidChars (c:cs)
--   | c `elem` "iol" = False 
--   | otherwise      = onlyValidChars cs

-- Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
findPair :: String -> (Bool, String)
findPair [] = (False, [])
findPair [c] = (False, [])
findPair (c0:c1:cs)
  | c0 == c1  = (True, cs)
  | otherwise = findPair (c1:cs)

hasTwoPairs :: String -> Bool
hasTwoPairs cs = foundFstPair && foundSndPair
  where
    (foundFstPair, remaining) = findPair cs
    (foundSndPair, _)         = findPair remaining

-- Stateful version of `hasTwoPairs`

statefulHasTwoPairsState :: String -> Bool 
statefulHasTwoPairsState = evalState [foundFstPair && foundSndPair | foundFstPair <- statefulFindPair, foundSndPair <- statefulFindPair]
  where
    statefulFindPair = state findPair

-- Putting the 3 checks together:
validPwd :: String -> Bool 
validPwd = [ x && y && z | x <- hasSequence, y <- onlyValidChars, z <- hasTwoPairs]

-- Solver
solveA :: String -> String 
solveA cs = let cs' = genNextCandidate cs in
  if validPwd cs' then cs'
  else genNextPwd cs'