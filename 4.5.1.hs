module Zy where

data Nat = Zero | Suc Nat deriving(Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero y = y
add (Suc px) y = add px (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero y = Zero
mul x Zero = Zero
mul (Suc px) y = add y (mul px y) 

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc px) = mul (Suc px) (fac px)


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node x y) = max (height x) (height y)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node x y) = size x + size y + 1