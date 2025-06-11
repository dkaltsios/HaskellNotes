h :: Int -> Int
h m = p [x | x <- [1..m], even x]
  where p []     = 0
        p (x:xs) = x + p xs  
        
        
data Student = St { firstName :: String
                  , lastName :: String
                  , sid :: Int } deriving (Show, Eq)        

largerLastNames :: [Student] -> [Int]        
largerLastNames students = [sid s | s <- students, length(lastName s)>7]

f1 :: Int -> IO ()
f1 n = mapM_ putStrLn [replicate i '*'| i<-[1..n]]

removeConsecutiveDuplicates :: Eq a => [a] -> [a]
removeConsecutiveDuplicates []  = []
removeConsecutiveDuplicates [x] = [x]
removeConsecutiveDuplicates (x:x1:xs)
    |x == x1   = removeConsecutiveDuplicates (x:xs)
    |otherwise = x:removeConsecutiveDuplicates (x1:xs) 

data Sudoku = Sudoku { rows :: [[Int]] } deriving (Show, Eq)

isFull :: Sudoku -> Bool
isFull (Sudoku rs) = all (all isFull) rs
  where 
    isFull 0 = False
    isFull _ = True

-- Convert int to list of digits
toDigits :: Int -> [Int]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Double every second digit starting from the right
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . zipWith ($) (cycle [id, (*2)]) . reverse

-- Subtract 9 from numbers > 9
fixDigits :: [Int] -> [Int]
fixDigits = map (\x -> if x > 9 then x - 9 else x)

-- Validate the card
isValidCard :: Int -> Bool
isValidCard n = total `mod` 10 == 0
  where
    digits = toDigits n
    transformed = fixDigits (doubleEveryOther digits)
    total = sum transformed
    
data Expr = Lit Int
  | Add Expr Expr
  | Mul Expr Expr    

pretty :: Expr -> String
pretty (Lit n) = show n

pretty (Add e1 e2) = pretty e1 ++ " + " ++ pretty e2

pretty (Mul e1 e2) =
  let showSub expr = case expr of
        Add _ _ -> "(" ++ pretty expr ++ ")"
        _       -> pretty expr
  in showSub e1 ++ " * " ++ showSub e2

main :: IO ()
main = do 
      f1 5
      let res = h 10
      putStrLn $ "Sum is "++ show res   
      
      