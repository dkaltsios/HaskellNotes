-- sumOfSquares x y = x^2 + y^2

inc n = n + 1

double n = n * 2

square n = n ^ 2

eval n = if n `mod` 2 == 0 
            then n-2 
            else 3*n+1

func n = (n * inc n)/2

implies True False = False
implies _ _ = True  

logEq True True = True
logEq False False = True
logEq _ _ = False

nand True True = False
nand _ _ = True

multipleOf :: Int -> Int -> Bool
multipleOf x y = if x `mod` y == 0
                    then True
                    else False

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)                    

ldigit n = n `mod` 10

diff n x y = max (max n x ) y - min (min n x ) y 