import System.TimeIt ( timeIt )
import Data.Char (digitToInt)
import Data.List ( foldl' )
import Control.Monad (guard)
-- For the Graph:
import Data.Array ( Ix(range), listArray, bounds, (!), Array )

-- The stateful types:

-- For accumulating the distance function:
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- For the points still to visit:
import Data.Set (Set)
import qualified Data.Set as Set

-- For Priority Queue
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap

-- # Types

-- Basic Types:
type Vertex   = (Int, Int)
type Distance = Int
type Graph    = Array Vertex Distance

-- Types used as a state:

type Distances = Map Vertex Distance -- gives the distance from Source
                                     -- Nothing means infinite

type ToVisit   = Set Vertex

type PQueue    = MinPrioHeap Distance Vertex

data DjikstraSt = St { dists :: Distances,
                       toVisit :: ToVisit,
                       pq :: PQueue}

-- Functions

cost :: Graph -> Vertex -> Distance
cost graph v = graph ! v

source :: Graph -> Vertex
source = fst . bounds

end :: Graph -> Vertex
end = snd . bounds

initSt :: Graph -> DjikstraSt
initSt graph = St {dists = Map.singleton (source graph) 0,
                   toVisit = Set.fromAscList (range $ bounds graph),
                   pq = Heap.singleton (0, source graph)}

neighbors :: DjikstraSt -> Vertex -> [Vertex]
neighbors st (x,y)
  = filter (`Set.member` toVisit st) neighbors'
  where
    neighbors' = concat [[(x + del, y), (x, y + del)] | del <- [-1,1]]

checkNeighbors :: Graph ->
                  Vertex -> 
                  Distance ->
                  DjikstraSt ->
                  DjikstraSt
checkNeighbors graph u dist_u st
  = foldl' checkNeighbor st (neighbors st u)
  where
    checkNeighbor st v
      | oldButGold = st
      | otherwise = st {dists = Map.insert v newDist (dists st),
                        pq    = Heap.insert (newDist, v) (pq st)}
      where
        newDist = dist_u + graph ! v
        oldButGold
          = case dists st Map.!? v of
            Nothing      -> False
            Just oldDist -> oldDist <= newDist

takeMin :: PQueue -> Maybe ((Distance, Vertex), PQueue)
takeMin = Heap.view

step :: Graph -> DjikstraSt -> Maybe DjikstraSt
step graph st
  = do
    ((dist_u, u), pq') <- takeMin (pq st)
    guard $ u /= end graph
    let st' = st {pq = pq', toVisit = Set.delete u (toVisit st)}
    if u `Set.notMember` toVisit st
      then return st'
      else return $ checkNeighbors graph u dist_u st'

dijkstra :: Graph -> Distance
dijkstra graph
  = aux (initSt graph)
  where
    aux st = case step graph st of
      Nothing  -> dists st Map.! end graph
      Just st' -> aux st'

-- parsing

parseEntry :: String -> Graph
parseEntry input = array
  where
    rawMatrix = lines input
    rows = length rawMatrix - 1
    cols = length (head rawMatrix) - 1
    preArray = digitToInt <$> concat rawMatrix
    array = listArray ((0,0), (rows,cols)) preArray

exInput :: IO Graph
exInput = parseEntry <$> readFile "./day15.ex.txt"

ioInput :: IO Graph
ioInput = parseEntry <$> readFile "./day15.input.txt"

-- part II

incRisk :: Distance -> Distance
incRisk 9 = 1
incRisk n = n + 1

parse2 :: String -> Graph
parse2 input = listArray ((0,0), (rows,cols)) preArray
  where
    rawMatrix = lines input
    rows = 5 * length rawMatrix - 1
    cols = 5 * length (head rawMatrix) - 1
    distMatrix = fmap (fmap digitToInt) rawMatrix
    increasedToLeft
      = concat . take 5 . iterate (fmap incRisk) <$> distMatrix
    increasedDown = concat $ take 5 $ iterate (fmap (fmap incRisk)) increasedToLeft
    preArray = concat increasedDown

exInput2 :: IO Graph
exInput2 = parse2 <$> readFile "./day15.ex.txt"

ioInput2 :: IO Graph
ioInput2 = parse2 <$> readFile "./day15.input.txt"

main :: IO ()
main = timeIt (ioInput2 >>= print . dijkstra)