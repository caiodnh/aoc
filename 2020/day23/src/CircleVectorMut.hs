module CircleVectorMut where

-- Imports:

import qualified Data.Vector.Mutable as V
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import CircleMut
import Data.List ( unfoldr )

-- Types:

newtype CircleVM = Cir (V.MVector (PrimState IO) Cup)

instance CircleIO CircleVM where
  (!) (Cir vector) (Cup n) = V.read vector n

  (//) (Cir vector)  = mapM_ (\(Cup i, Cup j) -> V.write vector i (Cup j))
    
    -- foldr f (return ())
    -- where
    --   f (Cup i, Cup j) _ = V.write @IO vector i (Cup j)

  prepareInput inputInt = do
    let inputCup = Cup . (`mod` size) <$> inputInt
    let inputPairs = zip inputCup (tail $ cycle inputCup)
    vector <- V.new size
    let circle = Cir vector
    circle // inputPairs
    -- (circle ! Cup 1) >>= print
    return (circle, head inputCup)

  printCircleIO circle = do
    let f cup = Just (cup, cup >>= (circle !))
    let cupVec = take size $ unfoldr f (return $ Cup 1)
    mapM (fmap show) cupVec >>= print