import Data.List.Split
import Control.Monad.State

data Command = U Int | D Int | F Int deriving Show

parseCommand :: String -> Command
parseCommand str = let [dir, textNum] = splitOn " " str
                       num = read textNum
                       in
                  case dir of
                    "forward" -> F num
                    "down"    -> D num
                    "up"      -> U num
                    _         -> error "Error parsing command."

followCommand :: Command -> State (Int, Int) ()
followCommand com = do
  (x, y) <- get 
  case com of
    F dx -> put (x + dx, y)
    D dy -> put (x, y + dy)
    U dy -> put (x, y - dy)
  return ()

followCommand' :: Command -> State (Int, Int) Char
followCommand' com = state f
  where
    f (x,y) = case com of
      F dx -> ('F', (x + dx, y))
      D dy -> ('D', (x, y + dy))
      U dy -> ('U', (x, y - dy))

ioInput :: IO [Command]
ioInput = fmap parseCommand . lines <$> readFile "./day02.txt"

-- solve1 :: [Command] -> Int
solve1 coms = runState (sequence $ fmap followCommand' coms) (0,0)

-- solution1 :: IO Int
solution1 = solve1 <$> ioInput

-- Part 2

followCommand2 :: Command -> State (Int, Int, Int) ()
followCommand2 com = do
  (x, y, aim) <- get 
  case com of
    F dx -> put (x + dx, y + aim * dx, aim)
    D dy -> put (x, y, aim + dy)
    U dy -> put (x, y, aim - dy)

solve2 :: [Command] -> Int
solve2 coms = let (x, y, aim) = execState (mapM followCommand2 coms) (0,0,0) in x * y

solution2 :: IO Int
solution2 = solve2 <$> ioInput