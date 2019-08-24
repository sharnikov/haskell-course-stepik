module Zyyyyyyyq where

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple 0 = []
pythagoreanTriple x = do 
        a <- [1..x]
        b <- [2..x]
        c <- [1..x]
        True <- return (a < b)
        True <- return (c <= x)
        return (a,b,c)
