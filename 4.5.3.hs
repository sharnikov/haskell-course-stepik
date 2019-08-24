module Zyy where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (Val e :*: e2) = if sp e2 then expand ((Val e) :*: expand e2) else (Val e) :*: e2
expand (e1 :*: Val e) = if sp e1 then expand (expand e1 :*: (Val e)) else e1 :*: (Val e)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = (expand e1 :*: expand e2)
expand e = e

sp :: Expr -> Bool
sp e = hasPlus (show e)

hasPlus :: String -> Bool
hasPlus [] = False
hasPlus s = if (\x -> not (x == '+')) $ head s then hasPlus (tail s) else True

v =  Val 1 :*: (Val 2 :*: (Val 3 :+: Val 4))

t0 = expand (Val 1 :+: Val 2)
t00 = expand (Val 1 :*: (Val 2 :+: Val 3))
t1 = expand ((Val 1 :+: Val 2):*:(Val 3 :+: Val 4))
t2 = expand (Val 1 :*: (Val 2 :+: Val 3) :*: Val 4)

t3 = expand ((Val 1 :+: Val 2) :*: Val 4 :*: Val 5)
t31 = expand ((Val 1 :+: Val 2) :*: Val 4 :*: Val 5 :*: Val 6)

t4 = expand (Val 1 :*: Val 2 :*: (Val 4 :+: Val 5))
t41 = expand (Val 1 :*: Val 2 :*: Val 3 :*: (Val 4 :+: Val 5))

t51 = expand ((Val 1 :+: Val 5 :+: Val 6) :*: (Val 2 :+: Val 3))
t52 = expand ((Val 1 :+: Val 5) :*: (Val 2 :+: Val 3 :+: Val 6))
t53 = expand ((Val 1 :+: Val 5 :+: Val 6) :*: (Val 2 :+: Val 3 :+: Val 4))
t54 = expand ((Val 1 :+: Val 5 :+: Val 6) :*: (Val 2 :+: Val 3 :+: Val 4) :*: Val 7)
t55 = expand ((Val 1 :+: Val 5 :+: Val 6) :*: (Val 2 :+: Val 3 :+: Val 4) :*: (Val 7 :+: Val 8))

t6 = expand 

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = undefined
    mappend = undefined

