module Zz where

data Odd = Odd Integer 
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

getNum :: Odd -> Integer
getNum (Odd n) = n

valka = Odd(13)
valka2 = Odd(15)
valka3 = Odd(101)

o1 = Odd(1)
o3 = Odd(3)
o7 = Odd(7)
o5 = Odd(5)
o11 = Odd(11)

cOdd :: Odd -> Odd -> Bool
cOdd x y = fromEnum x > fromEnum y

findInterval :: (Odd -> Odd) -> Odd -> Odd -> Int -> Int
findInterval nextStepF x y count = if x == y then count else findInterval nextStepF (nextStepF x) y (count + 1) 

nextStep :: (Odd -> Odd) -> Odd -> Int -> Int -> [Odd]
nextStep nextStepF x c i = if c == 0 then x : nextStep nextStepF (nextStepF x) (i - 1) i else nextStep nextStepF (nextStepF x) (c - 1) i

nextStepWithLimit :: (Odd -> Odd) -> Odd -> Int -> Int -> Odd -> [Odd]
nextStepWithLimit nextStepF element count interval limit = let 
							  					 	xzDistance = if cOdd element limit then findInterval pred element limit 0 else  findInterval succ element limit 0
												in if count > xzDistance then [] else (if count == xzDistance then [limit] else (if count == 0 then element : nextStepWithLimit nextStepF (nextStepF element) (interval - 1) interval limit else nextStepWithLimit nextStepF (nextStepF element) (count - 1) interval limit))

posEnumFromTo :: Odd -> Odd -> [Odd]
posEnumFromTo x y = if x == y then [x] else x : enumFromTo (succ x) y

instance Enum Odd where
	succ x = addEven x 2
	pred x = addEven x (-2)
	toEnum x = Odd (toInteger x)
	fromEnum x = fromInteger (getNum x)
	enumFrom x = [x] ++ enumFrom (addEven x 2) 
	enumFromThen x y = if cOdd x y then nextStep pred x 0 (findInterval pred x y 0) else nextStep succ x 0 (findInterval succ x y 0)
	enumFromTo x y = if cOdd x y then [] else posEnumFromTo x y
	enumFromThenTo x y z | cOdd x y && cOdd z x = [] 
	enumFromThenTo x y z | cOdd x z && cOdd y x = [] 
	enumFromThenTo x y z = if cOdd x y then (nextStepWithLimit pred x 0 (findInterval pred x y 0) z ) else (nextStepWithLimit succ x 0 (findInterval succ x y 0) z)