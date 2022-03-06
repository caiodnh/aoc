import Data.Sequence
import GHC.Exts.Heap (StgInfoTable(code))

test_input :: Code
test_input = fromList [1,9,10,3,2,3,11,0,99,30,40,50]

type Position = Int
type Value    = Int
type Code     = Seq Value

runStep :: Code -> Position -> (Code, Bool)
runStep code pos = case index code pos of
                     99 -> (code, True)
                     1  -> (newCode (+), False)
                     2  -> (newCode (*), False)
                  where
                    valueAt position = index code position
                    newCode op = update (valueAt (pos + 3)) (valueAt (valueAt (pos + 1)) `op` valueAt (valueAt (pos + 2)))code

runIntcode :: Code -> Code
runIntcode code = runAux code 0
  where
    runAux code pos = let (newCode, halt) = runStep code pos in
      if halt then newCode else runAux newCode (pos + 4)

prepareInput :: Code -> Code
prepareInput code0 = code2
  where
    code1 = update 1 12 code0
    code2 = update 2 2  code1

input :: Code
input = fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0]

solve :: Value
solve = index (runIntcode $ prepareInput input) 0

-- Part 2

level :: Int -> [(Int, Int)]
level n = Prelude.zip [0..n] (Prelude.reverse [0..n])

allPairs :: [(Int, Int)]
allPairs = concat (level <$> [0..])

prepareInput2 :: (Value, Value) -> Code -> Code
prepareInput2 (noun, verb) code0 = code2
  where
    code1 = update 1 noun code0
    code2 = update 2 verb code1

run2 :: (Value, Value) -> Value
run2 (noun, verb) = index (runIntcode $ prepareInput2 (noun, verb) input) 0

allResults :: [((Int, Int), Value)]
allResults = Prelude.zip allPairs (run2 <$> allPairs)

solve2 :: (Int, Int)
solve2 = auxSolve allResults
  where
    auxSolve list = let (pair, value) = head list in
      if value == 19690720 then pair
      else auxSolve $ tail list