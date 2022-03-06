import Data.IORef

addToCounter ::Int -> IORef Int -> IO ()
addToCounter num ref = modifyIORef ref (+ num)

addListToCounter :: [Int] -> IORef Int -> IO ()
addListToCounter []     ref = return ()
addListToCounter (n:ns) ref = addToCounter n ref >> addListToCounter ns ref

addAll :: [Int] -> IO Int
addAll ns = do
  ref <- newIORef 0
  addListToCounter ns ref
  readIORef ref

main :: IO ()
main = addAll [1, 10, 120] >>= print