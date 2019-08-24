module R where 

g :: Eq a => [a] -> [[a]]
g [] = []
g (x:[]) = [[x]]
g (x:xs) = if x == (head xs) then (x : head (g xs)) : (tail (g xs)) else [x] : (g xs)  
