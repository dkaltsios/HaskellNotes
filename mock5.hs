import Data.Char (toLower, isAlpha)

sum1 :: Num a => [a] -> a
sum1 = foldr (+) 0

reversef :: [a] -> [a]
reversef = foldr (\x xs -> xs ++ [x]) []

anyNew p = foldr (\x acc -> p x || acc) False

-- Lab starts here 

-- (\x -> x*10)
-- (\x -> x `elem`)

-- isVowel = \x -> x `elem` "aeiou"
isVowel = (`elem` "aeiou")

appendString = (++ "!" ) -- with partial

isEqual1 = \x -> x==' '

sumOdd x = sum (filter odd (filter (>10) x))

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 e x = length (filter (== e) x) >0

isPalindrome x = 
    let cl = map toLower (filter isAlpha x)
    in cl == reverse cl

sumOfSquares :: [Int] -> Int
sumOfSquares = foldr (\x acc -> x^2+acc) 0 

longestPalindrome :: [String] -> Maybe String
longestPalindrome l = case filter isPalindrome l of
    []   -> Nothing
    list -> Just (foldr seeL "" list) 
    where seeL x y = if length x > length y then x else y   

weightedSum :: [(Int, Int)] -> Int 
weightedSum pairs = sum [x*y | (x,y) <- pairs, even x]

excl True True   = False
excl False False = False
excl _ _         = True

xor :: [Bool] -> Bool
xor = foldr excl False


flatten :: [[a]] -> [a]
flatten a = foldr (++) [] a
    
evenLists :: [[a]] -> [[a]]
evenLists a = [x | x <- a, even (length x)]

allSafety :: (a -> Bool) -> [a] -> Bool
allSafety p = foldr (\x acc -> p x && acc) True

countHelp x [] = [(x,1)]
countHelp x ((y, n):ys)
   | x == y    = (y,n+1):ys
   | otherwise = (y,n):countHelp x ys

countOccurencies :: Eq a => [a] -> [(a,Int)]
countOccurencies = foldr countHelp []

