{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
-- import Control.Monad.Primitive
import Control.Monad ( forM_ )
import Data.Char (digitToInt)
import Data.List ( unfoldr )

import Time (time)

newtype Cup = Cup Int deriving (Eq)
newtype Circle = Cir (MV.IOVector Cup)

(!) :: Circle -> Cup -> IO Cup
(!) (Cir vector) (Cup n) = MV.read vector n

-- Instances:

instance Enum Cup where
  toEnum n = Cup ((n - 1) `mod` size + 1)
  fromEnum (Cup n) = n

instance Show Cup where
  show = show . fromEnum

-- instance Show Circle where
--   show circle = concatMap show $ take size $ unfoldr f (Cup 1)
--     where 
--       f cup = Just (cup, circle ! cup)

-- Constants: 

size :: Int
-- size = 10^6
size = 9

iterN :: Int
-- iterN = 10^7
iterN = 100

f :: String -> IO ()
f s = do
  print s

-- Functions

(//) :: Circle -> [(Cup,Cup)] -> IO ()
(Cir vec) // list
  = do
      print 2
      let f (Cup i, y) = MV.write vec i y
      forM_ list f

-- readInput :: [Int] -> (Circle, Current)
-- readInput input = (cir, cur)
--   where
--     cir = blank // pairs
--     blank = Cir $ V.replicate size (Cup 0)
--     pairs = zip cupInput (tail cupInput ++ [head cupInput])
--     cupInput = Cup <$> input
--     cur = head cupInput



readInput :: [Char] -> IO ()
readInput input = do
  let input' = cycle (adapt . digitToInt <$> input)
  let input'' = take size $ zip input' (tail input')
  print input''

  vector <- MV.new size
  let f (cup, nextCup) = MV.write vector cup nextCup
  forM_ input'' f

  print $ Cir vector

  v2 <- V.freeze vector
  print v2

-- No more V.x after this point

type Removed = [Cup]
type Current = Cup

pick :: Circle-> Current -> (Removed, Current)
pick cir current = fmap head $ splitAt 3 $ iterate (cir !) (cir ! current)

destination :: Current -> Removed -> Cup
destination oldCur removed
  = let dest = pred oldCur in
    if dest `elem` removed then destination dest removed
    else dest

updateCircle :: (Circle, Current) -> (Circle, Current)
updateCircle (oldCir, oldCur@(Cup n)) = (newCir, newCur)
  where
    newCir = oldCir // ([(oldCur, newCur), (dest, head removed), (last removed, oldCir ! dest)] ++ zip removed (tail removed))
    (removed, newCur) = pick oldCir oldCur
    dest = destination oldCur removed

multiplyStars :: Circle -> Int
multiplyStars circ
  = m * n
  where
    m = fromEnum (circ ! Cup 1)
    n = fromEnum (circ ! (circ ! Cup 1))

main :: IO ()

-- input :: Circle

input :: (Circle, Current)
input = readInput ( (digitToInt <$> "389125467") ++ [10..size])

solve :: Int -> (Circle, Current) -> (Circle, Current)
-- solve n cir = iterate update cir !! n
solve n (cir, cur) = iter (0, (cir, cur))
  where
    iter (i, (cir, cur))
      | i == n = (cir, cur)
      | otherwise = iter (i+1, updateCircle (cir, cur))

solve1 :: Int -> (Circle, Current)
solve1 n = solve n input

solve2 :: (Circle, Current) -> Int
solve2 = multiplyStars . fst . solve iterN

---



-- main :: IO ()
main = time $ print $ solve2 input