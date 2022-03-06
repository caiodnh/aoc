module CircleMap where

-- Imports:

import qualified Data.Map as Map
import Circle
import Data.List ( unfoldr )

-- Types:

newtype CircleM = CircleM {uncir :: Map.Map Cup Cup}

-- Instances:

instance Show CircleM where
  show circle = concatMap show $ take size $ unfoldr f (Cup 1)
    where 
      f cup = Just (cup, circle ! cup)

instance Circle CircleM where
  (!) circle cup = uncir circle Map.! cup
  (//) circle [] = circle
  (//) circle ((i, j):xs) = CircleM $ Map.update (\i -> Just j) i (uncir (circle // xs))
  prepareInput inputInt  = (cir, cur)
    where
      inputCup =  cycle $ Cup . (`mod` size) <$> inputInt
      cur = head inputCup
      inputPairs = take size $ zip inputCup (tail inputCup)
      cir = CircleM $ Map.fromList inputPairs