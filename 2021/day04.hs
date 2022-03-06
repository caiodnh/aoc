import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!), fromList, fromAscList, toList)
import Control.Monad.State
    ( evalState, execState, runState, MonadState(put, get), State )
import Data.List ( partition )
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, mapMaybe )

-- Types:

type Roll     = Int

type Position = (Int, Int)
type Board    = IntMap Position

type Count    = Int
type Columns  = IntMap Count
type Rows     = IntMap Count

-- # Parsing

ioInput :: IO ([Roll], [Board])
ioInput = parseEntry <$> readFile "./day04.txt"
  where
    parseEntry :: String -> ([Roll], [Board])
    parseEntry = do
      -- the first line are the numbers being rolled:
      x <- fmap read . splitOn "," . head . lines
      -- the following are the boards, separated by blank lines
      -- we need to remove the first line and the blank line after it
      y <- splitOn [""] . drop 2 . lines
      let z = fmap parseBoard y
      return (x, z)

    parseBoard :: [String] -> Board
    parseBoard = listBoardToBoard . listBoard
      where
        lineToInts :: String -> [Int]
        lineToInts = fmap read . filter (/= "") . splitOn " "

        listBoard :: [String] -> [[Int]]
        listBoard = fmap lineToInts

        listBoardToBoard :: [[Int]] -> Board
        listBoardToBoard lb = fromList $ zip (concat lb) positions
          where positions = [(x,y) | x <- [1..5], y <- [1..5]]

-- # Running just one board:

updateOneBoard :: Roll -> State (Board, Rows, Columns, Bool) (Maybe Int)
-- Input: the number that was just rolled
-- Output: the state machine that updates the board by removing the rolled number and marking if the game is over, and also returning the rolled number if it was found. The returning value was only used for debugging.
updateOneBoard num = do
  (board, rows, cols, bingo) <- get
  -- if already won, do nothing
  if bingo then return Nothing
  else do
  let pos = IntMap.lookup num board
  case pos of
    -- num is not in the board:
    Nothing -> return Nothing
    -- num is in the board:
    Just (row, col) -> do
      -- delete number from board
      let board' = IntMap.delete num board
      -- add one in the corresponding row
      let rows'  = IntMap.adjust (+1) row rows
      -- add one in the corresponding column
      let cols'  = IntMap.adjust (+1) col cols
      -- check if game is over
      let bingo' = rows' ! row == 5 || cols' ! col == 5
      -- update memory
      put (board', rows', cols', bingo')
      -- return the number that was found
      return $ Just num

initialRows :: Rows
initialRows = fromAscList (zip [1..5] (repeat 0))

initialCols :: Columns
initialCols = initialRows

initialGame :: Board -> (Board, Rows, Columns, Bool)
initialGame board = (board, initialRows, initialCols, False)

runGame :: [Int] -> Board -> (Board, [Int], Bool) -- Only for debugging
-- receives the list of number being rolled and a board
-- returns the board with numbers rolled removed, the list of found numbers and a Bool saying if the game was won or not.
runGame nums board = (board', found, bingo)
  where
    (rolls, (board', _, _, bingo)) = runState (mapM updateOneBoard nums) (initialGame board)
    found = catMaybes rolls

-- Running many boards together:

winnersOfTheRound :: Int -> State [(Board, Rows, Columns, Bool)] [(Roll, Board)]
winnersOfTheRound num = do
  boardStates <- get
  let updatedStates = fmap (execState (updateOneBoard num)) boardStates
  let (winStates, loseStates) = partition (\(b, r, c, won) -> won) updatedStates
  put loseStates
  let winners = fmap (\(b, r, c, w) -> b) winStates
  return $ zip (repeat num) winners

runAllGames :: ([Roll], [Board]) -> [(Roll, Board)]
runAllGames (rolls, boards) = concat $ evalState (mapM winnersOfTheRound rolls) (fmap initialGame boards)

firstWinner :: ([Roll], [Board]) -> (Roll, Board)
firstWinner = head . runAllGames

lastWinner :: ([Roll], [Board]) -> (Roll, Board)
lastWinner = last . runAllGames

winnerCode :: (Roll, Board) -> Int
winnerCode (num, board) = num * sum (fst <$> toList board)

solve1 :: ([Roll], [Board]) -> Int
solve1 = winnerCode . firstWinner

solution1 :: IO Int
solution1 = solve1 <$> ioInput

solve2 :: ([Roll], [Board]) -> Int
solve2 = winnerCode . lastWinner

solution2 :: IO Int
solution2 = solve2 <$> ioInput

