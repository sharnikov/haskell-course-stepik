module Oper where

import Data.Function

{-multSecond :: (t -> Int -> Int) -> (Int -> Int -> Int) -> Int-}
multSecond = g `on` h

g :: Int -> Int -> Int
g x y = x * y

h :: (t, Int) -> Int
h (x, y) = y