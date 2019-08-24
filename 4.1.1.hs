module Zzzzzz where

import Data.List

data Bit = Zero | One deriving (Show, Eq)
data Sign = Minus | Plus deriving (Show, Eq)
data Z = Z Sign [Bit] deriving (Show, Eq)

emptyZ = Z Plus []

convertZInt :: Z -> Int
convertZInt (Z s []) = 0
convertZInt (Z s bits) = (foldl (\sum (bit, count) -> sum + (if bit == One then 2^count else 0)) 0 mass) * sign where  
	sign = if s == Plus then 1 else (-1)
	mass = zip bits [0..(length bits)]

d :: Int -> Int -> Int
d x y = (fromIntegral (floor (xx / yy)))::Int where
	xx = realToFrac x
	yy = realToFrac y

m :: Int -> Int
m i = (fromIntegral (floor ( (realToFrac i) / 2)))::Int

com :: Int -> Bool
com i = (realToFrac i) / 2 >  realToFrac (m i)

convertIntZ :: Int -> Z
convertIntZ i = Z sign bits where 
 	sign = if i >=0 then Plus else Minus
 	bits = unfoldr (\value -> if value == 0 then Nothing else if com value then Just (One, m value) else Just (Zero, m value)) (abs i)


add :: Z -> Z -> Z
add x y = convertIntZ ((convertZInt x) + (convertZInt y))


mul :: Z -> Z -> Z
mul x y = convertIntZ ((convertZInt x) * (convertZInt y))


revertS :: Sign -> Sign
revertS Minus = Plus
revertS Plus = Minus

revert :: Bit -> Bit
revert One = Zero
revert Zero = One

addBit :: Z -> Bit -> Z
addBit (Z s bits) b = Z s (b:bits)

withSign :: Z -> Sign -> Z
withSign (Z fs bits) ts = Z ts bits

clean :: Z -> Z
clean (Z s []) = Z Plus []
clean (Z s b) = if (last b) == Zero then clean (Z s (init b)) else Z s b

-- add :: Z -> Z -> Z
-- add x y = clean $ ad x y 

ad :: Z -> Z -> Z
ad (Z s1 (x:[])) (Z s2 (y:[])) =
  case (s1, x, s2, y) of
  	(_, Zero, _, Zero) -> Z Plus [Zero]
  	(_, _, _, Zero)    -> Z s1 [x]
  	(_, Zero, _, _) -> Z s2 [y]
  	(Plus, One, Plus, One) -> Z Plus [Zero, One]
  	(Plus, One, Minus, One) -> Z Plus [Zero]
  	(Minus, One, Plus, One) -> Z Plus [Zero]
  	(Minus, One, Minus, One) -> Z Minus [Zero, One] 

ad (Z s1 []) (Z s2 []) = Z Plus []
ad (Z s1 xs) (Z s2 []) = Z s1 xs
ad (Z s1 []) (Z s2 ys) = Z s2 ys

ad (Z s1 ys) (Z s2 (x:[])) = ad (Z s2 [x]) (Z s1 ys)

ad (Z s1 (x:[])) (Z s2 ys) = 
	case ad (Z s1 [x]) (Z s2 [head ys]) of
		Z _ [Zero] -> Z s2 (Zero : (tail ys))
		Z s (b:[]) -> if s == s2 then Z s2 (b:(tail ys)) else withSign (addBit (add (Z s [One]) (Z s2 (tail ys))) b) s2
		Z s (b:bt) -> addBit (ad (Z s [One]) (Z s (tail ys))) Zero

ad (Z s1 xs) (Z s2 ys) =
    case ad (Z s1 [head xs]) (Z s2 ys) of
    	Z s (z:zs) -> addBit (ad (Z s1 (tail xs)) (Z s2 zs)) z

-- mul :: Z -> Z -> Z
-- mul (Z s1 xs) (Z s2 ys) = if bits == [] then Z Plus [] else clean (Z sign bits) where
-- 	sign = if s1 == s2 then Plus else Minus 
-- 	bits = mu xs ys

getBits :: Z -> [Bit]
getBits (Z s bits) = bits

mu :: [Bit] -> [Bit] -> [Bit]
mu [Zero] ys = []
mu xs [Zero] = []
mu [] ys = []
mu xs [] = []
mu (x:[]) (y:[]) = if x == One then [y] else []
mu ys (x:[]) = mu [x] ys 
mu (x:[]) ys = if x == One then ys else []
mu xs ys = getBits $ add first second where
	first = Z Plus (mu [head xs] ys)
	second = Z Plus (mu (tail xs) (Zero : ys))


test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul

zp = Z Plus [One, Zero, One, One]
zp2 = Z Plus [One, One, One, One]
zp3 = Z Plus [One, One, One, Zero]
zm = Z Minus [One, Zero, One, One]



