module Main where

-- # Imports

-- Vector
import qualified Data.Vector as V

-- Mutable Vector
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive (PrimMonad, PrimState)

-- Other
import Data.List ( unfoldr )
import Data.Char ( digitToInt )

-- Seno's time (work on windows)
import Time (time)

-- # Constants

-- ## Part 1

size :: Int 
size = 9

iterN :: Int
iterN = 100

-- ## Part 2

-- size :: Int 
-- size = 10^6

-- iterN :: Int
-- iterN = 10^7

-- ## Inputs

example :: String 
example = "389125467" 

actual :: String
actual = "398254716" 

-- # New types and instances:

-- ## Cup
newtype Cup = Cup Int deriving Eq

instance Enum Cup where
  toEnum n = Cup $ n `mod` size -- `size` is sent to 0 for because vectors' index start at 0
  fromEnum (Cup 0) = size
  fromEnum (Cup n) = n

instance Show Cup where
  show = show . fromEnum

-- ## Circle (immutable)

newtype Circle = Cir (V.Vector Cup)

nextInCir :: Circle -> Cup -> Cup
nextInCir (Cir vector) (Cup n) = vector V.! n

instance Show Circle where
  show circle =  show listOfCups
    where
      listOfCups = take size $ unfoldr f (Cup 1)
      f cup = Just (cup, nextInCir circle cup)
    
-- ## MCircle (mutable)

newtype MCircle s = MCir (MV.MVector s Cup)

nextInMCir :: PrimMonad m => MCircle (PrimState m) -> Cup -> m Cup
nextInMCir (MCir mVector) (Cup n) = MV.read mVector n

-- We can't have a Show instance because a MCircle lives inside a monad `m`.
-- We can have a `showMCir` returning a `m String`, but not a `String`.

showMCir :: PrimMonad m => MCircle (PrimState m) -> m String
showMCir mCircle = fmap show ioListOfCups
    where
      ioListOfCups = sequence listOfIOCups
      listOfIOCups = take size $ unfoldr f (return $ Cup 1)
      f ioCup = Just (ioCup, ioCup >>= nextInMCir mCircle)

-- Note that `print` should receive a `String` as argument, not a `IOString`.

printMCir :: MCircle (PrimState IO) -> IO ()
printMCir mCircle = showMCir mCircle >>= print

printMCirCur :: (MCircle (PrimState IO), Cup) -> IO ()
printMCirCur (mCircle, current) = showMCir mCircle >>= \string -> print (string, current)

-- # Functions: 

-- ## Change Values according to list of pairs:

changeValues :: Circle -> [(Cup, Cup)] -> Circle
changeValues (Cir vector) pairs = Cir $ vector V.// (clean <$> pairs)
  where
    clean (Cup i, Cup j) = (i, Cup j)

mChangeValues :: PrimMonad m => MCircle (PrimState m) -> [(Cup, Cup)] -> m ()
mChangeValues (MCir mVector) pairs = mapM_ f pairs
  where
    f (Cup i, Cup j) = MV.write mVector i (Cup j)

-- ## Read input:

readInput :: String -> (Circle, Cup)
readInput input = (circle, current)
  where
    inputInt = (digitToInt <$> input) ++ [10..size]
    inputCup = toEnum <$> inputInt
    current = head inputCup
    inputZip = zip inputCup (tail $ cycle inputCup)
    blankCir = Cir $ V.replicate size undefined -- the values start `undefined`
    circle = changeValues blankCir inputZip

mReadInput :: PrimMonad m => String -> m (MCircle (PrimState m), Cup)
mReadInput input = do
  let inputInt = (digitToInt <$> input) ++ [10..size]
  let inputCup = toEnum <$> inputInt
  let current = head inputCup
  let inputZip = zip inputCup (tail $ cycle inputCup)
  vector <- MV.new size -- the values are uninitiated
  let circle = MCir vector
  mChangeValues circle inputZip
  return (circle, current)

-- ## Pick the 3 cups just after the current one, and the future current cup

type Removed = (Cup, Cup, Cup)

pick :: (Circle, Cup) -> (Removed, Cup)
pick (circle, current)
  = ( (r1, r2, r3), current' )
  where
    r1 = nextInCir circle current
    r2 = nextInCir circle r1
    r3 = nextInCir circle r2
    current' = nextInCir circle r3

mPick :: PrimMonad m => (MCircle (PrimState m), Cup) -> m (Removed, Cup)
mPick (circle, current) = do
  r1 <- nextInMCir circle current
  r2 <- nextInMCir circle r1
  r3 <- nextInMCir circle r2
  current' <- nextInMCir circle r3
  return ( (r1, r2, r3), current' )

-- ## Find destination cup

destination :: Cup -> Removed -> Cup
destination current (r1, r2, r3)
  = let dest = pred current in
    if dest /= r1 && dest /= r2 && dest /= r3
      then dest
    else 
      destination dest (r1, r2, r3)

-- No need for a `mDestination`, we will also use `destination` in the mutable case

-- ## Update Circle

update :: (Circle, Cup) -> (Circle, Cup) -- pairs, because we will iterate this function
update (oldCir, oldCur) = (newCir, newCur)
  where
    newCir = changeValues oldCir [(oldCur, newCur), (dest, r1), (r3, nextInCir oldCir dest)]
    ((r1, r2, r3), newCur) = pick (oldCir, oldCur)
    dest = destination oldCur (r1, r2, r3)

mUpdate :: PrimMonad m => MCircle (PrimState m) -> Cup -> m Cup -- no pairs, because we won't
                                                                -- quite iterate this function
mUpdate circle oldCur
  = do
    ((r1, r2, r3), newCur) <- mPick (circle, oldCur)
    let dest = destination oldCur (r1, r2, r3)
    afterDest <- nextInMCir circle dest
    mChangeValues circle [(oldCur, newCur), (dest, r1), (r3, afterDest)]
    return newCur

-- ## Update `n` times

updateNTimes :: Int -> (Circle, Cup) -> (Circle, Cup)
updateNTimes n pair
  | n < 0     = error "iterating a negative amount of times"
  | n == 0    = pair
  | otherwise = updateNTimes (n-1) (update pair)

mUpdateNTimes :: PrimMonad m => Int -> MCircle (PrimState m) -> Cup -> m Cup
mUpdateNTimes n circle current
  | n < 0     = error "iterating a negative amount of times"
  | n == 0    = return current
  | otherwise = mUpdate circle current >>= \newCur -> mUpdateNTimes (n-1) circle newCur

-- ## Solve part 1

solve1 :: String -> String 
solve1 = show . fst . updateNTimes iterN . readInput

mSolve1 :: PrimMonad m => String -> m String
mSolve1 input = do
  (circle, current0) <- mReadInput input
  mUpdateNTimes iterN circle current0
  showMCir circle

-- ## Multiply content of 2 cups after cup 1

multiplyStars :: Circle -> Int
multiplyStars circle = m * n
  where
    cupM = nextInCir circle (Cup 1)
    cupN = nextInCir circle cupM
    m = fromEnum cupM
    n = fromEnum cupN

mMultiplyStars :: PrimMonad m => MCircle (PrimState m) -> m Int 
mMultiplyStars circle
  = do
    cupM <- nextInMCir circle (Cup 1)
    cupN <- nextInMCir circle cupM
    let m = fromEnum cupM
    let n = fromEnum cupN
    return $ m * n

-- ## Solve part 2

solve2 :: String -> String 
solve2 = show . multiplyStars . fst . updateNTimes iterN . readInput

mSolve2 :: String -> IO String 
mSolve2 input = do
  (circle, current0) <- mReadInput input
  mUpdateNTimes iterN circle current0
  ans <- mMultiplyStars circle
  return $ show ans

-- ## Main

main :: IO ()
main = time $ print =<< mSolve1 actual
  
