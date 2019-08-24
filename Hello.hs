main = putStrLn "Hello, world!"

f :: Integer -> Integer
f n | n == 0    = 0
    | n == 1    = 1
    | n == (-1) = 1
    | n > 0     = f (n - 1) + f (n - 2)
    | n < 0     = (-f (n + 1)) + f (n + 2)

f1 :: Integer -> Integer
f1 n | n == 0    = 0
     | n == 1    = 1
     | n == (-1) = 1
     | n > 0     = hp 0 1 2 n
     | n < 0     = hn 0 1 2 (abs n)
    
hp n1 n2 c n = if c == n then n1 + n2 else hp n2 (n1 + n2) (c + 1) n
hn n1 n2 c n = if c == n then n2 - n1 else hn n2 (n1 - n2) (c + 1) n


                  
                    