module Tdi where	

fc :: Double -> Double
fc x = 10.0

idi :: Double -> Double
idi x = x

i :: (Double -> Double) -> Double -> Double -> Double
i f a b = h f 0 0 ((b - a) / 1000) a

h :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double
h f c s t a = let 
					xi = a + c * t
					xi1 = xi + t
					result = ((f xi + f xi1) / 2) * t
            in if c == 999.0 then (s + result) else h f (c + 1) (s + result) t a

i1 :: (Double -> Double) -> Double -> Double -> Double
i1 f a b = (h1 f 0 0 ((b - a) / 1000)) * (if b >= a then 1 else (-1))

h1 :: (Double -> Double) -> Double -> Double -> Double -> Double
h1 f c s t = let 
					xi = c * t
					xi1 = xi + t
					result = ((f xi + f xi1) / 2) * t
            in if c == 999.0 then (s + result) else h1 f (c + 1) (s + result) t 





i2 :: (Double -> Double) -> Double -> Double -> Double
i2 f a b = let 
			t = (b - a) / 1000
			x1 = a + t
			fa = f a / 2 * (x1 - a)
			xn = b - (a + 999 * t)
			fb = f b / 2 * (b - xn)
           in h2 f 1 0 t

h2 :: (Double -> Double) -> Double -> Double -> Double -> Double
h2 f c s t = let 
					xi = c * t
					result = f xi / 2 * t 
            in if c == 999 then (s + result) else h2 f (c + 1) (s + result) t 

