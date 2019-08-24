module Zyyyyyy where

data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s = \x -> Log [s] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers v f1 f2 = let 
  l1 x = l3 $ l2 $ f1 x
  l2 (Log s vv) = (f2 vv, s)  
  l3 (Log xs vvv, s) = Log (s ++ xs) vvv
 in l1 v

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"