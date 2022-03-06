import Data.Vector ( (!?), Vector, fromList )
import Data.Maybe ( mapMaybe, maybeToList)
import Control.Monad ( guard )
-- for Part 2 only:
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.List ( sortOn )

type Pos       = (Int, Int)
type Height    = Int
type RiskLevel = Int

type Chart     = Vector (Vector Height)

checkHeight :: Chart -> Pos -> Maybe Height
checkHeight chart (a, b) = do
  row <- chart !? a
  row !? b

data Arrow = V Int Int deriving (Show, Eq)

(+>) :: Pos -> Arrow -> Pos
(a, b) +> (V x y) = (a + x, b + y)

directions :: [Arrow]
directions = [V 0 1, V 1 0, V (-1) 0, V 0 (-1)]

isLow :: Chart -> Pos -> Maybe Bool
isLow chart pos = do
  let adjPos     = fmap (pos +>) directions :: [Pos]
  let adjHeights = mapMaybe (checkHeight chart) adjPos :: [Height]
  h <- checkHeight chart pos
  return $ all (h <) adjHeights

risk :: Chart -> Pos -> Maybe RiskLevel
risk chart pos = do
  continue <- isLow chart pos
  guard continue
  h <- checkHeight chart pos
  return $ h + 1

allPos :: (Int, Int) -> [Pos]
allPos (rows, cols) = [(x, y) | x <- [0..rows-1], y <- [0..cols-1]]

solve1 :: ((Int, Int), Chart) -> RiskLevel
solve1 ((rows, cols), chart) = sum $ mapMaybe (risk chart) (allPos (rows, cols))

parseInput :: String -> ((Int, Int), Chart)
parseInput rawInput = ((rows, cols), chart)
  where
   listInput :: [[Height]]
   listInput = fmap (read . return) <$> lines rawInput
   rows = length listInput
   cols = length $ head listInput
   chart = fromList $ map fromList listInput

ioInput :: IO ((Int, Int), Chart)
ioInput = parseInput <$> readFile "./day09.txt"

ioTestInput :: IO ((Int, Int), Chart)
ioTestInput =  parseInput <$> readFile "./day09.test.txt"

solution1 :: IO RiskLevel
solution1 = solve1 <$> ioInput

-- Part 2

type Basin       = Set Pos
type StateBasin  = State Basin ()
type StateBasins = State [Basin] ()

increaseBasin :: Chart -> Pos -> StateBasin
increaseBasin chart pos
  = let maybeHeight = checkHeight chart pos
    in case maybeHeight of
      Nothing -> state $ \b -> ((), b)
      Just 9  -> state $ \b -> ((), b)
      Just h  -> do
        basin <- get
        let basin' = pos `Set.insert` basin
        let nextCandidates = fmap (pos +>) directions
        let nextPositions  = filter (`Set.notMember` basin') nextCandidates
        put basin'
        mapM_ (increaseBasin chart) nextPositions

findBasin :: Chart -> Pos -> Basin
findBasin chart pos = execState (increaseBasin chart pos) Set.empty

addToBasins :: Chart -> Pos -> StateBasins
addToBasins chart pos = do
  basins <- get
  if any (pos `Set.member`) basins
    then put basins
  else
    do
      let newBasin = findBasin chart pos
      put $ newBasin : basins

findAllBasins :: ((Int, Int), Chart) -> [Basin]
findAllBasins ((rows, cols), chart)
  = filter (/= Set.empty) $ execState stateAllBasins []
  where
    stateAllBasins = mapM_ (addToBasins chart) (allPos (rows, cols))

sortedBasinsSizes :: [Basin] -> [Int]
sortedBasinsSizes = sortOn ((-1) *) . fmap Set.size

mult3Biggest :: [Int] -> Int
mult3Biggest = product . take 3

solve2 :: ((Int, Int), Chart) -> Int
solve2 = mult3Biggest . sortedBasinsSizes . findAllBasins

solution2 :: IO Int
solution2 = solve2 <$> ioInput

