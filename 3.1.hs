module Az where

d = \x -> (f . (g . h)) x

f x = logBase 2 x

g x = x ^ 3

h x = max x 42