module Q where

data Odd = Odd Integer 
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

getNum :: Odd -> Integer
getNum (Odd n) = n

makeTillUpper :: [Odd] -> [Odd]
makeTillUpper x = 


instance Enum Odd where
	succ x = addEven x 2
	pred x = addEven x 2
	toEnum x = Odd (toInteger x)
	fromEnum x = fromInteger (getNum x)
	enumFrom :: Enum a => a -> [a]
    enumFrom x = makeTillUpper [x]

    enumFromThen :: Enum a => a -> a -> [a]
    enumFromThenTo :: Enum a => a -> a -> a -> [a]
    enumFromTo :: Enum a => a -> a -> [a]
