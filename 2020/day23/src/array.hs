import qualified Data.Array as A
import Data.Char (digitToInt)
import Debug.Trace ( trace )
import Data.List ( unfoldr )

import System.CPUTime (getCPUTime)
import Text.Printf (printf)

newtype Cup = Cup Int deriving (Eq, Ord, A.Ix)
newtype Circle = Cir (A.Array Cup Cup)

(!) :: Circle -> Cup -> Cup
(!) (Cir vector) (Cup n) = vector A.! Cup n

-- Instances:

instance Enum Cup where
  toEnum n = Cup ((n - 1) `mod` size + 1)
  fromEnum (Cup n) = n

instance Show Cup where
  show = show . fromEnum

instance Show Circle where
  show circle = concatMap show $ take size $ unfoldr f (Cup 1)
    where 
      f cup = Just (cup, circle ! cup)

size :: Int
size = 10^6
iterN :: Int
iterN = 10^7

readInput :: [Int] -> Circle
readInput input = Cir (A.array (Cup 1, Cup size) pairs)
  where
    pairs = zip cupInput (tail cupInput ++ [head cupInput])
    cupInput = map Cup input

type Removed = [Cup]
type Current = Cup

pick :: Circle -> Current -> (Removed, Current)
pick cir current = fmap head $ splitAt 3 $ iterate (cir !) (cir ! current)

destination :: Current -> Removed -> Cup
destination oldCur removed
  = let dest = pred oldCur in
    if dest `elem` removed then destination dest removed
    else dest

updateCircle :: (Circle, Current) -> (Circle, Current)
updateCircle (oldCir@(Cir array), oldCur@(Cup n)) = (newCir, newCur)
  where
    newCir = Cir $ array A.// ([(oldCur, newCur), (dest, head removed), (last removed, oldCir ! dest)] ++ zip removed (tail removed))
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
input = readInput ( (digitToInt <$> "389125467") ++ [10..size])

solve :: Int -> (Circle, Current) -> (Circle, Current)
-- solve n cir = iterate update cir !! n
solve n (cir, cur) = iter (0, (cir, cur))
  where
    iter (i, (cir, cur))
      | i == n = (cir, cur)
      | otherwise = iter (i+1, updateCircle (cir, cur))

solve1 :: Int -> (Circle, Current)
solve1 n = solve n (input, Cup 3)

solve2 :: (Circle, Current) -> Int
solve2 = multiplyStars . fst . solve iterN

---

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v

-- main :: IO ()
main = time $ print $ solve2 (input, Cup 3)