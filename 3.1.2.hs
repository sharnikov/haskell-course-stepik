module Pp where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = s x (s y z)

s :: Num a => [a] -> [a] -> [a]
s (x:xs) (y:[]) = (x + y) : xs
s (x:[]) (y:ys) = (x + y) : ys
s (x:xs) (y:ys) = (x + y) : s xs ys
s [] y = y
s x [] = xу 