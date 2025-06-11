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

-- isPalindrome x = 