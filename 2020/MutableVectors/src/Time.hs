module Time where

import System.CPUTime (getCPUTime)
import Text.Printf (printf)

time :: IO t -> IO t
time a = do
  putStrLn "Starting..."
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  putStrLn "Done!"
  let diff = fromIntegral (end - start) / (10 ^ 12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v