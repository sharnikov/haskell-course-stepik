module Pr where 

x = [1,2,3]
z = [2,3]

p :: Eq a => [a] -> [[a]]
p [] = [[]]
p (x:[]) = [[x]]
p x = concatMap (\e -> add e (p (fbl e x) ) ) x

add :: Eq a => a -> [[a]] -> [[a]]
add e x = map (\t -> e : t) x

fbl :: Eq a => a -> [a] -> [a]
fbl e x = concatMap (\l -> if (l == e) then [] else [l]) x 


pre :: Eq a => [a] -> [[a]]
pre [] = [[]]
pre (x:[]) = [[x]]
pre x = concatMap (\e -> map (\t -> e : t) (pre (concatMap (\l -> if (l == e) then [] else [l]) x) ) ) x




perms :: [a] -> [[a]]
perms [] = [[]]
perms x = h [] x

h :: [a] -> [a] -> [[a]]
h [] (x:[]) = [[x]]
h (x:[]) (y:[]) = [[y, x]]
h x (y:[]) = map (y:) (h [] x)
h x y = (map ((head y):) (h [] (x ++ (tail y)))) ++ (h (x ++ [head y]) (tail y))