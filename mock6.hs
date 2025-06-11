while :: (a->Bool) -> (a->a) -> a -> a
while check update value  
    | check value = while check update (update value) 
    | otherwise     = value


invert :: [Bool] -> [Bool]
invert = map not 

add1 :: Int -> Int
add1 x = x+1

square :: Int -> Int
square x = x*x

multiplyBy10 :: Int -> Int
multiplyBy10 x = x*10

f = multiplyBy10 . square . add1

doubleMap f g = map (f . g)

parts :: (a -> Bool) -> [a] -> ([a],[a])
parts p list = (filter p list, filter (not . p) list)

f1 = subtract 5 . (/2) . (`min` 100)

count p xs = length (filter p xs)

sumLists :: [[Int]] -> Int
sumLists x = sum (concat x) 

insertOrder :: (Ord a) => a -> [a] -> [a]
insertOrder e [] = [e]
insertOrder e (x:xs) 
    | e <= x    = (e:x:xs)
    | otherwise = x : insertOrder e xs

isort :: (Ord a) => [a] -> [a]
isort = foldr insertOrder []    

blend xs ys = concat (zipWith (\x y-> [x,y]) xs ys) 

findInd :: (a->Bool) -> [a] -> Maybe Int
findInd p xs = search (zip [0..] xs)
    where 
        search [] = Nothing
        search ((i,x):rest) 
            | p x = Just i
            | otherwise = search rest 

mismatches a b = length(filter not (zipWith (==) a b))       

groupByKey :: Eq k => [(k,v)]->[(k,[v])]
groupByKey = foldr helperKey []

helperKey (k,v) [] = [(k,[v])] --If given rest of list is empty return just this sole pair
helperKey (k,v) ((a,vs):rest)  --Take pair and rest kv pairs and if they match add them
    | k == a = (k, v:vs) : rest
    | otherwise = (a,vs) : helperKey (k,v) rest

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = let rest = subsequences xs in rest ++ map (x:) rest 

isSorted l = and(zipWith (<=) l (tail l))
