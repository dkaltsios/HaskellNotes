-- f :: Int -> String -> Int -> Float
-- f x s y = fromIntegral (x + y + length s)

-- add :: Int -> Int -> Int
-- add x y = x + y

-- addOne = add 1

-- func x = addOne x

-- power :: Integer -> Integer -> Integer
-- power _ k | k<0 = error "negative argument given" 
-- power _ 0       = 1
-- power n k       = n * power n (k-1)   



-- calc m n = multiplyBy10 (double (add m n))
--   where 
--     add x y = x+y
--     double x = x*2
--     multiplyBy10 x = x*10

calc m n =  let a = m+n
                b = a*2
            in b*10

myNot a = case a of
    True  -> False
    False -> True

percentDiff :: Double -> Double -> Int
percentDiff a b = round(multiplyBy100 ((absoluteDiff a b)/((add a b)/2)))
    where 
        add a b = a+b
        absoluteDiff a b = abs (a-b)
        multiplyBy100 a = a*100 

    
mySum :: Int -> Int
mySum 1 = 1
mySum x = x + mySum(x-1)    

myHead (x:xs) = x

-- rev :: [a] -> [a]
-- rev []     = []
-- rev (x:xs) = rev xs ++ [x]
myTail (x:xs) = myHead (reverse(x:xs)) 


secondLast (x:_:[]) = x
secondLast (_:xs)   = secondLast xs 
secondLast _        = error "hi"

listOr :: [Bool] -> Bool 
listOr [] = False
listOr (x:xs) = x || listOr xs 


add10 :: [Int] -> [Int]
add10 [] = []
add10 (x:xs) = x+10 : add10 xs

elementAt (x:xs) i = (x:xs)!!(i-1)

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

lastDigit a = a`mod`10

sumDigits 0 = 0
sumDigits x = lastDigit x + sumDigits(x`div`10)     
-- Questions about the exam. 
-- Should we write the function signatures as well
-- Can we use build in function 
-- Should we use main etc?

evenPositions :: [Int] -> [Int]
evenPositions []       = []
evenPositions (x:[])   = [x]
evenPositions (x:_:xs) = x : evenPositions xs

ackermann m n | m == 0 = n+1
              | m > 0 && n==0 = ackermann (m-1) 1
              | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))

collatz n 
    | n<1 = undefined
    | otherwise = go n 0
    where go 1 steps = steps
          go x steps
             | even x    = go (x`div`2) (steps +1)
             | otherwise = go (3*x + 1) (steps +1)

