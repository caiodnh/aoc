import Data.Map ( Map, (!))
import qualified Data.Map as Map

type Pair = (Char, Char)

-- Parse

parseEntry :: String -> (Map Pair Char, String)
parseEntry input = (instructions, initial)
  where
    rows = lines input
    initial = head rows
    rawInstructions = drop 2 rows
    parseRow row = let [a, b] = take 2 row in ((a,b), row !! 6)
    listInstructions = fmap parseRow rawInstructions
    instructions = Map.fromList listInstructions

ioExample :: IO (Map Pair Char, String)
ioExample = parseEntry <$> readFile "./day14.ex.txt"

ioInput :: IO (Map Pair Char, String)
ioInput = parseEntry <$> readFile "./day14.input.txt"

-- Part 1:

findPairs :: String -> [Pair]
findPairs []       = []
findPairs [_]      = []
findPairs (a:b:cs) = (a,b) : findPairs (b:cs)

increasePair :: Map Pair Char -> Pair -> String
increasePair instructions (a,b)
  = let c = instructions Map.! (a,b) in [c,b]

step :: Map Pair Char -> String -> String
step instructions initial
  = head initial : concatMap (increasePair instructions) (findPairs initial)

runNSteps :: Int -> Map Pair Char -> String -> String
runNSteps n instr init = iterate (step instr) init !! n

frequencies :: String -> Map Char Int 
frequencies str = Map.unionsWith (+)  listOfMaps
  where
    init = zip str $ repeat 1
    listOfMaps = fmap (Map.fromList . return) init

mostFrequent :: Map Char Int -> Int
mostFrequent = Map.foldr max 0

leastFrequent :: Map Char Int -> Int
leastFrequent = minimum . Map.foldr (:) []

solve1 :: (Map Pair Char, String) -> Int
solve1 (instr, init) = mostFrequent freq - leastFrequent freq
  where
    freq = frequencies $ runNSteps 10 instr init

-- Part 2

newPairs :: Map Pair Char -> Pair -> (Pair, Pair)
newPairs instr (a,b) = let c = instr ! (a,b) in ((a,c), (c, b))

type Count = Int

makeCountPairs :: String -> Map Pair Count 
makeCountPairs str = Map.unionsWith (+) listOfMaps
  where
    listOfMaps = Map.fromList . return <$> zip (findPairs str) (repeat 1)

step' :: Map Pair Char -> Map Pair Count -> Map Pair Count
step' instr before = Map.unionWith (+) leftPairs rightPairs
  where
    leftPairs  = Map.mapKeysWith (+) (fst . newPairs instr) before
    rightPairs = Map.mapKeysWith (+) (snd . newPairs instr) before

runNSteps' :: Int -> Map Pair Char -> String -> Map Pair Count 
runNSteps' n instr init = iterate (step' instr) (makeCountPairs init) !! n

mapOfCharacters :: Char -> Char -> Map Pair Int -> Map Char Int -- the fst and snd characters are counted only one, the others twice
mapOfCharacters a b mapOfPairs = Map.map (`div` 2) allChars  
  where
    leftChars  = Map.unionWith (+) (Map.mapKeysWith (+) fst mapOfPairs)
                                   (Map.fromList [(a, 1)])
    rightChars = Map.unionWith (+) (Map.mapKeysWith (+) snd mapOfPairs)
                                   (Map.fromList [(b, 1)])
    allChars   = Map.unionWith (+) leftChars rightChars

-- solve2 :: (Map Pair Char, String) -> Int
solve2 (instr, init) = mostFrequent freq - leastFrequent freq
  where
    a    = head init
    b    = last init
    freq = mapOfCharacters a b $ runNSteps' 40 instr init

-- Debug

instr :: Map Pair Char
instr = Map.fromList [(('B','B'),'N'),
                      (('B','C'),'B'),
                      (('B','H'),'H'),
                      (('B','N'),'B'),
                      (('C','B'),'H'),
                      (('C','C'),'N'),
                      (('C','H'),'B'),
                      (('C','N'),'C'),
                      (('H','B'),'C'),
                      (('H','C'),'B'),
                      (('H','H'),'N'),
                      (('H','N'),'C'),
                      (('N','B'),'B'),
                      (('N','C'),'B'),
                      (('N','H'),'C'),
                      (('N','N'),'C')] 

state0 :: Map (Char, Char) Int
state0 = Map.fromList [(('C','B'),1),(('N','C'),1),(('N','N'),1)]

