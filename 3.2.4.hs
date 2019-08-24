module Rr where 

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 x y z = zipWith3 (\x y z -> max x (max $ y z)) x y z