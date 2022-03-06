module CircleVector where

import qualified Data.Vector as V
import Circle ( Circle(..), Cup(..), size )
import Data.List ( unfoldr )

newtype CircleV = Cir (V.Vector Cup)

instance Show CircleV where
  show circle = concatMap show $ take size $ unfoldr f (Cup 1)
    where 
      f cup = Just (cup, circle ! cup)

instance Circle CircleV where
  (!) (Cir vector) (Cup n) = vector V.! n

  (Cir vector) // list   = Cir $ vector V.// (clean <$> list)
    where clean (Cup n, Cup m) = (n, Cup m)

  prepareInput inputInt  = (cir, cur)
    where
      cir = blank // pairs
      blank = Cir $ V.replicate size (Cup 0)
      pairs = zip cupInput (tail cupInput ++ [head cupInput])
      cupInput = Cup . (`mod` size) <$> inputInt
      cur = head cupInput


-- (!) :: Circle -> Cup -> Cup

-- -- Instances:

-- instance Enum Cup where
--   toEnum n = Cup ((n - 1) `mod` size + 1)
--   fromEnum (Cup n) = n

-- instance Show Cup where
--   show = show . fromEnum

-- size :: Int
-- -- size = 10^6
-- size = 9

-- iterN :: Int
-- -- iterN = 10^7
-- iterN = 100

-- (//) :: Circle -> [(Cup,Cup)] -> Circle
-- (Cir vec) // list = Cir (vec V.// list')
--   where
--     list' = clean <$> list
--     clean (Cup x, Cup y) = (x, Cup y)

-- readInput :: [Int] -> (Circle, Current)
-- readInput input = 

-- -- No more V.x after this point

-- type Removed = [Cup]
-- type Current = Cup

-- pick :: Circle -> Current -> (Removed, Current)
-- pick cir current = fmap head $ splitAt 3 $ iterate (cir !) (cir ! current)

-- destination :: Current -> Removed -> Cup
-- destination oldCur removed
--   = let dest = pred oldCur in
--     if dest `elem` removed then destination dest removed
--     else dest

-- updateCircle :: (Circle, Current) -> (Circle, Current)
-- updateCircle (oldCir, oldCur@(Cup n)) = (newCir, newCur)
--   where
--     newCir = oldCir // ([(oldCur, newCur), (dest, head removed), (last removed, oldCir ! dest)] 
--       ++ zip removed (tail removed))
--     (removed, newCur) = pick oldCir oldCur
--     dest = destination oldCur removed

-- multiplyStars :: Circle -> Int
-- multiplyStars circ
--   = m * n
--   where
--     m = fromEnum (circ ! Cup 1)
--     n = fromEnum (circ ! (circ ! Cup 1))

-- main :: IO ()

-- -- input :: Circle

-- input :: (Circle, Current)
-- input = readInput ( (digitToInt <$> "389125467") ++ [10..size])

-- solve :: Int -> (Circle, Current) -> (Circle, Current)
-- -- solve n cir = iterate update cir !! n
-- solve n (cir, cur) = iter (0, (cir, cur))
--   where
--     iter (i, (cir, cur))
--       | i == n = (cir, cur)
--       | otherwise = iter (i+1, updateCircle (cir, cur))

-- solve1 :: Int -> (Circle, Current)
-- solve1 n = solve n input

-- solve2 :: (Circle, Current) -> Int
-- solve2 = multiplyStars . fst . solve iterN

-- -- main :: IO ()
-- main = time $ print $ solve2 input