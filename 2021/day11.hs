{-# LANGUAGE TupleSections, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

-- Containers: 

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVec

-- other imports:
import Control.Monad
import Data.Char (digitToInt)
import Control.DeepSeq
import GHC.IO (unsafePerformIO)

-- Types:

newtype Energy = E Int deriving (Eq, Ord)

instance Show Energy where
  show (E n) = show n

type Pos   = (Int, Int)
type Chart = Vector (IOVector Energy)

ioPrint :: Chart -> IO ()
ioPrint chart = do
  putStrLn "-----"
  Vec.forM_ chart printRow
  where
    printRow row = forM_ [0..MVec.length row - 1] printVal >> putStrLn ""
      where
        printVal i = do
          val <- MVec.read row i
          putStr $ show val

readRow :: Int -> [Energy] -> IO (IOVector Energy)
readRow size row = do
  vecRow <- MVec.new size
  foldM_ (f vecRow) 0 row
  return vecRow
  where
    f vecRow pos energy = do
    MVec.write vecRow pos energy
    return (pos + 1)
      

-- Parsing:
-- TODO escrever com mapM
-- DONE escrever com seq
readRow' :: Int -> [Energy] -> IO (IOVector Energy)
readRow' size row = do
  vecRow <- MVec.new size
  let n = foldl (addToVec' vecRow) 0 row
  seq n (return vecRow)
    where
      addToVec' :: IOVector Energy -> Int -> Energy -> Int
      addToVec' vecRow pos energy = seq (unsafePerformIO action) (pos + 1)
        where
          action = MVec.write @IO vecRow pos energy

      addToVec :: IOVector Energy -> IO Int -> Energy -> IO Int
      addToVec vecRow ioPos energy = do
        pos <- ioPos
        -- let a = MVec.write vecRow pos energy
        -- a
        MVec.write vecRow pos energy
        return (pos + 1)

parseEntry :: String -> IO Chart
parseEntry input = Vec.fromList <$> listOfMVec'
  where
    charMatrix   = lines input
    nCols        = length (head charMatrix)
    energyLine   = fmap (E . digitToInt)
    energyMatrix = energyLine <$> charMatrix
    listOfMVec   = forM energyMatrix (readRow nCols) :: IO [IOVector Energy]
    listOfMVec'  = mapM' (readRow nCols) energyMatrix

-- (>>=) :: IO a -> (a -> IO b) -> IO b
mapM' :: forall a b . (a -> IO b) -> [a] -> IO [b]
mapM' f []     = return []
mapM' f (a:as) = do
  first <- f a :: IO b
  rest  <- mapM' f as :: IO [b]
  return (first : rest)

sequence' :: [IO a] -> IO [a]
sequence' []         = return []
sequence' (ioA:ioAs) = do
  a  <- ioA
  as <- sequence' ioAs
  return (a:as)

mapM'' f as = sequence' $ fmap f as

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

raisePosition :: Chart -> Pos -> IO ()
raisePosition chart (x, y)
  = case chart Vec.!? x of
    Nothing   -> fail "Column out of range."
    Just mvec -> MVec.modify mvec raiseEnergy y

raiseRow :: IOVector Energy -> IO [Int]
raiseRow row = concat <$> mapM raiseAndRecord [0..MVec.length row - 1]
  where
    raiseAndRecord :: Int -> IO [Int]
    raiseAndRecord i = do
      MVec.modify row raiseEnergy i
      val <- MVec.read row i
      return [i | val == E 10]

raiseAll :: Chart -> IO [Pos]
raiseAll chart = concat <$> mapM raiseAndRecord [0..Vec.length chart - 1]
  where
    raiseAndRecord :: Int -> IO [Pos]
    raiseAndRecord i = fmap (i,) <$> raiseRow (chart Vec.! i)

-- flashing:

gainEnergy :: Energy -> (Energy, Bool) -- the Bool is if it is flashing
gainEnergy (E 9)  = (E 0, True)
gainEnergy (E 10) = (E 0, True)
gainEnergy (E 0)  = (E 0, False)
gainEnergy (E n)  = (E $ n + 1, False)

tryFlashing :: Chart -> Pos -> IO Bool
tryFlashing chart (x, y)
  = case chart Vec.!? x of
    Nothing  -> return False
    Just row -> if y < 0 || y >= MVec.length row
                then return False
                else do
                  oldValue <- MVec.read row y
                  let (newValue, isFlashing) = gainEnergy oldValue
                  MVec.write row y newValue
                  return isFlashing

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
        flashing <- tryFlashing chart p
        if flashing
          then aux (ps ++ neighbors p) (alreadyFlashed + 1)
          else aux ps alreadyFlashed 

-- Solving:

step :: Chart -> IO Count
step chart = do
  toFlash <- raiseAll chart
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
  let total = Vec.length chart * MVec.length (chart Vec.! 0)
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