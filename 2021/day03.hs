import Data.List (transpose)
import Data.Char (digitToInt)

type Bits = [Int] -- Each bit is 0 or 1, a Int

mostFrequent :: Bits -> Int 
mostFrequent list = let total = length list
                        ones  = sum list in
                          if 2 * ones >= total
                            then 1
                            else 0

gammaBinnary :: [Bits] -> [Int]
gammaBinnary = fmap mostFrequent . transpose

epsilonBinnary :: [Bits] -> [Int]
epsilonBinnary = fmap (1-) . gammaBinnary

binnary2dec :: [Int] -> Int
binnary2dec = foldl f 0
  where
    f :: Int -> Int -> Int
    f acc digit = 2 * acc + digit

solve1 :: [Bits] -> Int 
solve1 = do
  x <- binnary2dec . gammaBinnary
  y <- binnary2dec . epsilonBinnary
  return $ x * y

parseEntry :: String -> [Bits]
parseEntry = fmap readBits . lines
  where
    readBits :: String -> Bits
    readBits = fmap digitToInt

ioInput :: IO [Bits]
ioInput = parseEntry <$> readFile "./day03.txt"

solution1 :: IO Int
solution1 = solve1 <$> ioInput

-- Part2

bitsAndNumber :: [Bits] -> [(Bits, Int)]
bitsAndNumber = fmap (\x -> (x, binnary2dec x))

filterByFirstBit :: ([Bits] -> Bits) -> [(Bits, Int)] -> Int
filterByFirstBit f listOfPairs
  = case listOfPairs of
      [(_, num)] -> num
      []         -> error "Never got to only one element in the list."
      _          -> filterByFirstBit f newListOfPairs
        where
          newListOfPairs = (\(b:bs, num) -> (bs, num)) <$>
            filter (\(b:bs, _) -> b == compBit) listOfPairs
          listOfBits = fmap fst listOfPairs
          compBit = head $ f listOfBits

genRating :: ([Bits] -> Bits) -> [Bits] -> Int
genRating f input = filterByFirstBit f (bitsAndNumber input)

oxiRating :: [Bits] -> Int
oxiRating = genRating gammaBinnary
co2Rating :: [Bits] -> Int
co2Rating = genRating epsilonBinnary

solve2 :: [Bits] -> Int
solve2 = do
  x <- oxiRating
  y <- co2Rating
  return $ x * y





