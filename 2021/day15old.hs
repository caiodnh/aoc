import Data.Array.IO
import Data.Array
import GHC.Exts (sortWith)

import Data.Char (digitToInt)

data Risk = R Int | Infinity deriving (Eq, Ord, Show)

type Pos = (Int, Int)
-- type VTable    = IOArray Pos Risk
type RiskTable   = Array Pos Risk
type StepTable   = RiskTable
type FutureTable = RiskTable

data Direction = V Int Int deriving (Eq, Show)

-- Part 1:

(+>) :: Pos -> Direction -> Pos
(x, y) +> V dx dy = (x + dx, y + dy)

directions :: [Direction]
directions = [V dx 0 | dx <- [-1,1]] ++ [V 0 dy | dy <- [-1,1]]

-- accumRisk :: VTable -> Pos -> IO Risk
-- accumRisk vtable pos
--   = do
--     bounds <- getBounds vtable
--     if inRange bounds pos
--       then readArray vtable pos
--       else return Infinity

readRisk :: RiskTable -> Pos -> Risk
readRisk stepTable pos
  = if inRange (bounds stepTable) pos
      then stepTable ! pos
      else Infinity

add :: Risk -> Risk -> Risk
add (R x) (R y) = R $ x + y
add _     _     = Infinity

-- Essentialy, Q :: (Pos, Direction) -> Risk
q :: StepTable -> FutureTable -> Pos -> Direction -> Risk
q stepTable vTable pos dir
  = add stepRisk futureRisk
  where
    pos' = pos +> dir
    stepRisk   = readRisk stepTable pos'
    futureRisk = readRisk vTable pos'

newFutureRisk :: StepTable -> FutureTable -> Pos -> Risk
newFutureRisk stepTable vTable pos
  = minimum $ fmap (q stepTable vTable pos) directions

newFutureTable :: StepTable -> FutureTable -> FutureTable
newFutureTable stepTable vTable
  = listArray extremes list
  where
    extremes = bounds stepTable
    list  = fmap (newFutureRisk stepTable vTable) (range extremes)

initialFuture :: StepTable -> FutureTable
initialFuture stepTable
  = listArray extremes $ replicate (size -1) Infinity ++ [R 0]
  where
    extremes = bounds stepTable
    size     = rangeSize extremes

runUntilStable :: StepTable -> FutureTable -> RiskTable
runUntilStable stepTable vTable
  = let newVTable = newFutureTable stepTable vTable
    in
    if newVTable == vTable
      then newVTable
      else runUntilStable stepTable newVTable

path :: RiskTable -> [Pos]
path riskTable
  = reverse $ aux [start]
  where
    (start, end) = bounds riskTable
    aux oldPath
      = let pos = head oldPath
        in
        if pos == end
          then oldPath
          else nextPos pos : aux oldPath
    nextPos pos = head $ sortWith (riskTable !) ((pos +>) <$> directions)

solve1 :: StepTable -> Risk
solve1 stepTable
  =  readRisk (runUntilStable stepTable (initialFuture stepTable)) (0,0)

-- Parsing:

parseEntry :: String -> StepTable
parseEntry input
  = listArray ((0,0), (rows, cols)) riskList
  where
    rawMatrix = lines input
    (rows, cols) = (length rawMatrix - 1, length (head rawMatrix) - 1)
    riskList = fmap (R . digitToInt) (concat rawMatrix)

ioExample = parseEntry <$> readFile "./day15.ex.txt"

ioInput   = parseEntry <$> readFile "./day15.txt"