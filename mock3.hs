-- evenOnly :: [Int] -> [Int]
-- evenOnly [] = []
-- evenOnly (x:xs) 
--     | x`mod`2==0 = x : evenOnly xs
--     | otherwise = evenOnly xs


scalarproduct :: [Int] -> [Int] -> Int
scalarproduct [] []         = 0
scalarproduct (x:xs) (y:ys) = x*y + scalarproduct xs ys     

removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]
-- [expression| generator, condition]

firsts px = [x | (x,_) <- px]

-- Haskell is lazy, it delays computation until actually needing it

powerOfTwoAndThree :: Int -> [(Int, Int)]
powerOfTwoAndThree n = [(power2, power3)| value <- [1..n]
                                        , let power2 = value^2
                                        , let power3 = value ^3]

-- To ask him, should we write it online since computers dont have haskell installed
count1 :: Char -> String -> Int
count1 x xs = length [x1 | x1<-xs, x1==x]

quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a<-xs, a>x]
    in smaller ++ [x] ++ bigger 

    
-- lab 3 starts here --    

isNil [] = True
isNil _ = False

odds :: Int -> [Int] 
odds n = [x | x<-[1..n], x`mod`2==1]

replic :: Int -> a -> [a] 
replic 0 _ = [] 
replic n a = a : replic (n-1) a

myconcat :: [[a]] -> [a]
myconcat xs = foldr (++) [] xs

evenOnly n = [x | x<-[1..n], x`mod`2==0]

factors n = [x | x<-[1..n], n`mod`x==0]

cprod xs ys = [(x,y)| x<-xs, y<-ys]

sq x = x^2
-- we have too a^2+b^2=c^2
pythTriples :: Int -> [(Int,Int,Int)]
pythTriples x = [(a,b,c) | 
                          a<-[1..x],
                          b<-[1+a..x],
                          c<-[1+b..x],
                          sq a + sq b == sq c]

interspace :: a -> [a] -> [a] 
interspace _ [] = []
interspace _ [x] = [x]
interspace e (x:xs) = x : e : interspace e (xs)                          


numMismatches :: Eq a => [a] -> [a] -> Int
numMismatches [] [] = 0
numMismatches (x:xs) (y:ys)
    | x==y = numMismatches xs ys
    | otherwise = 1 + numMismatches xs ys


noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups [x] = [x]
noDups (x:xs) | x`elem`xs = noDups xs
              | otherwise = x : noDups xs

splitIn2 :: Int -> [a] -> ([a],[a])
splitIn2 n xs = (take n xs, drop n xs)               

-- lcs :: Eq a => [a] -> [a] -> [a]
