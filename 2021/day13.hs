import Data.Set
import qualified Data.Set as Set
import Data.Bifunctor

import Data.List.Split

type Point = (Int, Int)
type Paper = Set Point

type Line  = (Char, Int)

-- Parsing

parseEntry :: String -> (Paper, [Line])
parseEntry input = (paper, foldingLines)
  where
    rows = lines input :: [String]
    [rawPaper, rawFoldingLines] = splitOn [""] rows
    listToPoint [x,y] = (read x, read y) :: Point
    listToPoint _     = error "Not a point."
    paper = Set.fromList $ listToPoint . splitOn "," <$> rawPaper
    readLine line = (line !! 11, read $ Prelude.drop 13 line)
    foldingLines = fmap readLine rawFoldingLines

ioExample :: IO (Paper, [Line])
ioExample = parseEntry <$> readFile "day13.ex.txt"

ioInput :: IO (Paper, [Line])
ioInput = parseEntry <$> readFile "day13.txt"

-- Part 1:

foldAlongAxis :: Line -> Paper -> Paper
foldAlongAxis (axis, x0) paper = left `union` flipedRight
  where
    (proj, applyToEntry) = case axis of
      'x' -> (fst, first)
      'y' -> (snd, second)
      _   -> error "Not a valid axis."
    (left, right) = Set.partition ((<= x0) . proj) paper
    flipedRight   = Set.map (applyToEntry (2 * x0 -)) right

solve1 :: (Paper, [Line]) -> Int
solve1 (paper, lines) = size $ foldAlongAxis (head lines) paper

-- Part 2:

printPaper :: Paper -> IO ()
printPaper paper = mapM_ putStrLn matrix
  where
    list = toAscList paper
    maxRow = maximum $ fmap snd list :: Int
    maxCol = maximum $ fmap fst list :: Int
    matrix = [[f (x,y) | x <- [0..maxCol]] | y <- [0..maxRow]]
    f point
      | point `member` paper = '#'
      | otherwise            = '.'
    
solve2 :: (Paper, [Line]) -> Paper
solve2 (paper, lines) = applyFolds paper
  where
    folds = foldAlongAxis <$> reverse lines
    applyFolds = Prelude.foldr (.) id folds

solve2' :: Foldable t => (Paper, t Line) -> Paper
solve2' (paper, lines) = Prelude.foldl (flip foldAlongAxis) paper lines
