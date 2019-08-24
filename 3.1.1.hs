module Aa where 

p :: Eq a => [a] -> Bool
p (x:[]) = True
p (x:xs) = if x == (last xs) then p (init xs) else False
p [] = True