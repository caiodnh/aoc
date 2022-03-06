import Data.Array.IO
import Control.Monad ( forM_, forM )
import Data.Char (digitToInt)

-- Types:

newtype Energy = E Int deriving (Eq, Ord)

instance Show Energy where
  show (E n) = show n

type Pos   = (Int, Int)
type Chart = IOArray Pos Energy

ioPrint :: Chart -> IO ()
ioPrint chart = do
  putStrLn "-----"
  ((row0, col0), (row1, col1)) <- getBounds chart
  let printEntry i j = readArray chart (i, j) >>= putStr . show
  let printRow   i   = forM_ [col0..col1] (printEntry i) >> putStrLn ""
  forM_ [row0..row1] printRow
  putStrLn "-----"

-- Parsing:

parseEntry :: String -> IO Chart
parseEntry input = newListArray ((1,1), (rows, cols)) energyMatrix
  where
    charMatrix   = lines input
    rows         = length charMatrix
    cols         = length (head charMatrix)
    energyMatrix = E . digitToInt <$> concat charMatrix

ioInput :: IO Chart
ioInput = readFile "./day11.txt" >>= parseEntry

ioTestInput :: IO Chart
ioTestInput = readFile "./day11.test.txt" >>= parseEntry

ioSmallTestInput :: IO Chart
ioSmallTestInput = readFile "./day11.smalltest.txt" >>= parseEntry

-- # Part 1:

-- Raising 1 for every position:

raiseEnergy :: Energy -> Energy
raiseEnergy (E n) = E $ n + 1

raiseAll :: Chart -> IO ()
raiseAll chart = do
  associates <- getAssocs chart
  forM_ associates add1
    where
      add1 :: (Pos, Energy) -> IO ()
      add1 (pos, val) = writeArray chart pos (raiseEnergy val)

collect10s :: Chart -> IO [Pos]
collect10s chart = fmap fst . filter ( (== E 10). snd) <$> getAssocs chart

-- flashing:

boltEnergy :: Energy -> (Energy, Bool) -- the Bool is if it is flashing
boltEnergy (E 9)  = (E 0, True)
boltEnergy (E 10) = (E 0, True)
boltEnergy (E 0)  = (E 0, False)
boltEnergy (E n)  = (E $ n + 1, False)

boltPosition :: Chart -> Pos -> IO Bool
boltPosition chart pos
  = do
    allPos <- getBounds chart
    if inRange allPos pos
      then
        do
        oldVal <- readArray chart pos
        let (newValue, isFlashing) = boltEnergy oldVal
        writeArray chart pos newValue
        return isFlashing
      else
        return False

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + dx, y + dy) |
                    dx <- [-1..1],
                    dy <- [-1..1],
                    (dx, dy) /= (0,0)]

type Count = Int

flashList :: Chart -> [Pos] -> IO Count
flashList chart toFlash = aux toFlash 0
  where
    aux [] alreadyFlashed 
      = return alreadyFlashed
    aux (p:ps) alreadyFlashed
      = do
        flashing <- boltPosition chart p
        if flashing
          then aux (ps ++ neighbors p) (alreadyFlashed + 1)
          else aux ps alreadyFlashed 

-- Solving:

step :: Chart -> IO Count
step chart = do
  raiseAll chart
  toFlash <- collect10s chart
  ans <- flashList chart toFlash
  -- ioPrint chart
  return ans

runNSteps :: Chart -> Int -> IO Count
runNSteps chart n = sum <$> mapM (const $ step chart) [1..n]

solve1 :: Chart -> IO Count
solve1 chart = runNSteps chart 100

solution1 :: IO Count
solution1 = ioInput >>= solve1

-- Part 2

step' :: Chart -> IO Bool
step' chart = do
  totalFlashing <- step chart
  ((x0,y0), (x1,y1)) <- getBounds chart
  let total = (x1 - x0 + 1) * (y1 - y0 + 1)
  return $ totalFlashing == total

solve2 :: Chart -> IO Int
solve2 chart = aux 0
  where
    aux n = do
      all <- step' chart
      if all then return (n+1)
      else aux (n+1)

solve2' :: Chart -> IO Int
solve2' chart = length . takeWhile not <$> mapM (const $ step' chart) [1..500]