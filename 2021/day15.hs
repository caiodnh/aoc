import Control.Monad.State
import Algorithm.Search
import System.TimeIt

-- For accumulating the distance function:
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- For the table of costs:
import Data.Array

-- For the Priority Queue:
import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Char (digitToInt)
import GHC.Integer (integerToInt)
import Data.Maybe (fromJust)

-- # Types

-- Basic Types:
type Vertex   = (Int, Int)
data Distance = Fin Int | Infinite deriving (Show, Eq, Ord)
type Costs = Array Vertex Distance

instance Semigroup Distance where
  (Fin x) <> (Fin y) = Fin $ x + y
  _       <> _       = Infinite

-- Types used as a state:

type Distances = Map Vertex Distance -- gives the distance from Source

type ToVisit   = Set Vertex

type PQueue    = IntMap (Set Vertex) -- never maps to the empty set


-- Priority Queue and Distances updates:

updatePQueue :: PQueue -> Vertex -> Distance -> Distance -> PQueue
updatePQueue pq vertex oldDist newDist
  = case newDist of
    Infinite -> pq -- We are supposing that updates always *decrease* the priority, so a infinite distance does not change anything
    Fin n    -> pq'
      where
      -- remove vertex from it
      removedFromOldSet = case oldDist of
        Infinite -> pq -- Infinite distances are never in the queue
        Fin m    -> IntMap.update removeFromSet m pq
      removeFromSet set
        = let newSet = Set.delete vertex set
            in if newSet == Set.empty
                then Nothing -- If the set becomes empty, it should be removed. See Set.update.
                else Just newSet
      -- Put vertex in the new set
      pq' = case removedFromOldSet IntMap.!? n of
        Nothing -> IntMap.insert n (Set.singleton vertex) removedFromOldSet
        Just _  -> IntMap.adjust (Set.insert vertex) n removedFromOldSet

updateDistances :: Distances -> Vertex -> Distance -> Distances
updateDistances dists vertex newDist
  = Map.adjust (const newDist) vertex dists

removeMin :: PQueue -> Maybe (Distance, Vertex, PQueue)
removeMin pq = do
  (dist_int, vertex_set) <- IntMap.lookupMin pq
  let (vertex, newSet) = Set.deleteFindMin vertex_set
  let maybeSet = if newSet == Set.empty
                  then Nothing
                  else Just newSet
  let pq' = IntMap.update (const maybeSet) dist_int pq
  return (Fin dist_int, vertex, pq')

updateWithNeighbor :: Costs -> Distance -> Vertex -> State (ToVisit, Distances, PQueue) ()
updateWithNeighbor costs dist_u v
  = do
    (toVisit, dists, pq) <- get
    let dist_v = dists Map.! v
    let dist_v' = dist_u <> costs ! v
    if dist_v' < dist_v
      then put ( toVisit
               , updateDistances dists v dist_v'
               , updatePQueue pq v dist_v dist_v' )
      else put (toVisit, dists, pq)

step :: Costs -> (ToVisit, Distances, PQueue) -> Maybe (ToVisit, Distances, PQueue)
step costs (toVisit, dists, pq) = do
  (dist_u, u@(x,y), pq') <- removeMin pq
  guard $ u /= snd (bounds costs)
  let toVisit' = Set.delete u toVisit
  let pre_neighbors = concat [[(x + delta, y), (x, y + delta)] | delta <- [-1,1]]
  let neighbors = filter (`Set.member` toVisit') pre_neighbors
  let forLoop = mapM_ (updateWithNeighbor costs dist_u) neighbors
  return $ execState forLoop (toVisit', dists, pq')

runTillStop :: Costs -> (ToVisit, Distances, PQueue) -> Distances
runTillStop costs triple@(x,y,z)
  = case step costs triple of
    Nothing      -> y
    Just triple' -> runTillStop costs triple'

initState :: Costs -> (ToVisit, Distances, PQueue)
initState costs
  = (toVisit, dists, pq)
  where
    toVisit = Set.fromAscList $ indices costs
    dists = Map.fromList $ zip (indices costs) (Fin 0 : repeat Infinite)
    pq = IntMap.singleton 0 (Set.singleton (0,0))

solve1 :: Costs -> Distance
solve1 costs
  = runTillStop costs (initState costs) Map.! snd (bounds costs)

-- parsing

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

main :: IO ()
main = timeIt(ioInput2 >>= print . solve1)

-- using library

neighbors :: Costs -> Vertex -> [Vertex]
neighbors cost (x, y)
  = filter (inRange (bounds cost)) candidates
  where
    candidates
      = concat [[(x + delta, y), (x, y + delta)] | delta <- [-1,1] ]

solve' :: Costs -> Distance
solve' cost = fst $ fromJust $ dijkstra (neighbors cost) (\x y -> cost ! y) (== snd (bounds cost)) (fst (bounds cost))

instance Num Distance where
  (Fin a) + (Fin b) = Fin $ a + b
  fromInteger = Fin . fromInteger

solve'' :: Costs -> Distance
solve'' cost = fst $ fromJust $ aStar (neighbors cost) (\x y -> cost ! y) manhattan (== snd (bounds cost)) (fst (bounds cost))
  where
    manhattan (x, y) = let (a, b) = snd (bounds cost) in Fin $ a - x + b - y