module Zzzz where

import Data.List 

l = [1.0,2.0,3.0,4.0]

f :: (Double, Double) -> Double
f (x, 0) = 0
f (x, y) = x / y

meanList :: [Double] -> Double
meanList l = f $ foldr (\element (sum, count) -> (sum + element, count + 1)) (0, 0) l

g :: Integer -> [a] -> [a]
g index [] = []
g index (x:xs) = if not $ odd index then x : (g (index + 1) xs) else (g (index + 1) xs)


evenOnly :: [a] -> [a]
evenOnly l = g 1 l

