module Oper where

infixl 6 *+* 
a *+* b = a ^ 2 + b ^ 2

foo :: Bool -> Int
foo ~True = 1