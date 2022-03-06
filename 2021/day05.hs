import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split ( splitOn )

data Point = P Int Int deriving (Eq, Ord)
data Vec   = V Int Int deriving Eq

instance Show Point where
  show (P a b) = show (a, b)

direction :: Point -> Point -> Vec
direction (P a b) (P c d) = V x' y'
  where
    x  = c - a
    y  = d - b
    t  = gcd x y
    x' = x `quot` t
    y' = y `quot` t

(+>) :: Point -> Vec -> Point
(P a b) +> (V x y) = P (a + x) (b + y)

pointsBetween :: Point -> Point -> [Point]
pointsBetween p q = aux p q (direction p q)
  where
    aux p q v
      | p == q    = [p]
      | otherwise = p : aux (p +> v) q v

horOrVert :: Vec -> Bool
horOrVert (V x y) = x == 0 || y == 0

makeSegment :: (Point, Point) -> Map Point Int 
makeSegment (p, q) = Map.fromList $ zip (pointsBetween p q) (repeat 1)

mergeSegments :: [Map Point Int] -> Map Point Int 
mergeSegments = Map.unionsWith (+)

findBadPoints :: Map Point Int -> [Point]
findBadPoints = fmap fst . Map.toList . Map.filter (> 1)

parallel :: (Point, Point) -> Bool 
parallel (P a b, P c d) = a == c || b == d

solve1 :: [(Point, Point)] -> Int
solve1 = length . findBadPoints . mergeSegments . fmap makeSegment . filter parallel

parseInput :: String -> [(Point, Point)]
parseInput = fmap readSegment . lines
  where
    breakLine = splitOn " -> "
    readPoint = f . splitOn ","
      where
        f [a,b] = let x = read a :: Int
                      y = read b :: Int
                      in P x y
        f _     = error "parsing pair"
    readSegment = g . fmap readPoint . breakLine
      where
        g [p, q] = (p, q)
        g _      = error "parsing segment"

ioInput = parseInput <$> readFile "./day05.txt"

solution1 = solve1 <$> ioInput

solve2 :: [(Point, Point)] -> Int
solve2 = length . findBadPoints . mergeSegments . fmap makeSegment 

solution2 = solve2 <$> ioInput