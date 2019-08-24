module Zyyyyyyyqqtb where

import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = do
 (x, y) <- get 
 put (y, x + y)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
