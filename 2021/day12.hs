import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map, (!) )
import qualified Data.Map as Map

import Control.Monad ( guard )
import Control.Monad.State
import qualified Data.List as List
import Data.List.Split ( splitOn )

-- types

data Cave = Small String | Big String deriving (Eq, Ord)

instance Show Cave where
  show (Small x) = show x
  show (Big x)   = show x

type CleanPath = [Cave] -- list of Caves visited, in REVERSE order

type Path = (CleanPath, Set Cave)

type Neighbors = [Cave]
type Graph = Map Cave Neighbors

--

ended :: Path -> Bool
ended = (== Small "end") . head . fst

increasePaths :: Graph -> [Path] -> [Path]
increasePaths graph pastPaths
  = do
    (path, notAgain) <- pastPaths
    let currentCave = head path
    nextCave <- graph ! currentCave
    guard $ nextCave `Set.notMember` notAgain
    let path' = nextCave : path
    let notAgain' = case nextCave of
                    Small _ -> Set.insert nextCave notAgain
                    Big   _ -> notAgain
    return (path', notAgain')

-- step :: Graph -> State [Path] [CleanPath]
-- -- returns a State returning the completed Paths
-- step graph
--   = do
--     pastPaths <- get
--     let newPaths = increasePaths graph pastPaths
--     let (finished, notFinished) = List.partition ended newPaths
--     put notFinished
--     return $ fmap fst finished

step :: Graph -> ([Path], [CleanPath]) -> ([Path], [CleanPath])
step graph (notCompleted, completed)
  = (stillNotCompleted, completed ++ newCompleted)
  where
    newPaths = increasePaths graph notCompleted
    (newCompleted', stillNotCompleted) = List.partition ended newPaths
    newCompleted = fmap fst newCompleted'

findAllPaths :: Graph -> [CleanPath]
findAllPaths graph
  = exhaust initialState
  where
    initialState :: ([Path], [CleanPath])
    initialState
      = ([([Small "start"], Set.singleton (Small "start"))], [])

    exhaust :: ([Path], [CleanPath]) -> [CleanPath]
    exhaust (notCompleted, completed)
      = if null notCompleted
        then completed
        else exhaust $ step graph (notCompleted, completed)

solve1 :: Graph -> Int
solve1 = length . findAllPaths

-- Parsing

parseName :: String -> Cave
parseName name
  | head name `elem` ['a'..'z'] = Small name
  | otherwise                   = Big name

makeArrow :: [Cave] -> Graph
makeArrow [a,b] = Map.fromList [(a, [b]), (b, [a])]
makeArrow _     = error "Parsing arrow"

parseEntry :: String -> Graph
parseEntry input = Map.unionsWith (++) mapArrows
  where
    parseLine = fmap parseName . splitOn "-"
    listArrows = parseLine <$> lines input
    mapArrows = fmap makeArrow listArrows

ioSmallEx :: IO Graph
ioSmallEx = parseEntry <$> readFile "./day12.ex1.txt"
ioMedEx :: IO Graph
ioMedEx = parseEntry <$> readFile "./day12.ex2.txt"
ioBigEx :: IO Graph
ioBigEx = parseEntry <$> readFile "./day12.ex3.txt"
ioInput :: IO Graph
ioInput = parseEntry <$> readFile "./day12.input.txt"

-- Part 2

increasePaths' :: Graph -> [(Path, Bool)] -> [(Path, Bool)]
increasePaths' graph pastPaths
  = do
    ((path, smallVisited) , sameSmallTwice) <- pastPaths
    let currentCave = head path
    nextCave <- graph ! currentCave
    let visited = nextCave `Set.member` smallVisited
    guard $ not ( (visited && sameSmallTwice) || nextCave == Small "start")
    let path' = nextCave : path
    let smallVisited' = case nextCave of
                    Small _ -> Set.insert nextCave smallVisited
                    Big   _ -> smallVisited
    let sameSmallTwice' = sameSmallTwice || visited
    return ((path', smallVisited'), sameSmallTwice')

step' :: Graph -> ([(Path, Bool)], [CleanPath]) -> ([(Path, Bool)], [CleanPath])
step' graph (notCompleted, completed)
  = (stillNotCompleted, completed ++ newCompleted)
  where
    newPaths = increasePaths' graph notCompleted
    (newCompleted', stillNotCompleted) = List.partition (ended . fst) newPaths
    newCompleted = fmap (fst . fst) newCompleted'

findAllPaths' :: Graph -> [CleanPath]
findAllPaths' graph
  = exhaust initialState
  where
    initialState :: ([(Path, Bool)], [CleanPath])
    initialState
      = ([(([Small "start"], Set.singleton (Small "start")), False)], [])

    exhaust :: ([(Path, Bool)], [CleanPath]) -> [CleanPath]
    exhaust (notCompleted, completed)
      = if null notCompleted
        then completed
        else exhaust $ step' graph (notCompleted, completed)

solve2 :: Graph -> Int
solve2 = length . findAllPaths'
