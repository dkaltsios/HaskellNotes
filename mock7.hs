-- _ Custom Eq

data Pet = D | C | P
instance Eq Pet where 
    D == D = True
    C == C = True
    P == P = True
    _ == _ = False

instance Show Pet where 
    show D = "Dog"
    show C = "Cat"
    show P = "Parrot"

-- instance Ord Date where 
--     compare (Date y1 m1 d1) (Date y2 m2 d2) = ...   

-- Order Class
-- Equality Class
-- Eq to find and Show to print

data Title = Mr | Mrs | Ms | Dr | Prof

data Name = Name { title :: Title
                 , first :: String
                 , last  :: String
                 }

instance Show Title where 
    show Mr   = "Mr."
    show Mrs  = "Mrs."
    show Ms   = "Ms."
    show Dr   = "Dr."
    show Prof = "Prof."

instance Show Name where 
    show (Name t f l)= show t ++ " " ++ f ++ " " ++ l                 

data Student = St {
  firstName :: String ,
  lastName  :: String ,
  sId       :: Int } deriving ( Show )

instance Eq Student where 
    (St _ _ s1) == (St _ _ s2) = (s1==s2)

instance Ord Student where 
    compare (St _ l1 _) (St _ l2 _) = compare l1 l2

data NonEmptyList a = NonEmpty a [a]

head1 (NonEmpty x _) = x

tail1 (NonEmpty _ xs) = xs

maxList :: Ord a => [a] -> a
maxList (x:xs) = foldr (max) x xs

-- data List a = Empty | Cons a (List a)
data FileSystem 
  = File {fname :: String, size :: Double} 
  | Directory {dname :: String, contents :: [FileSystem]}
  deriving (Show)

sizeOf :: FileSystem -> Double
sizeOf (File _ size) = size
sizeOf (Directory _ items) = sum (map sizeOf items)

-- JUST TO TEST 
-- exampleFS :: FileSystem
-- exampleFS = Directory "root"
--     [ File "file1.txt" 100.5
--     , File "file2.txt" 200.0
--     , Directory "subdir"
--         [ File "file3.txt" 300.25
--         , Directory "nested"
--             [ File "file4.txt" 400.75 ]
--         ]
--     ]

data Tree a = Node a ( Tree a ) ( Tree a ) | Empty
  deriving ( Show ) 

treeSum :: Num a => Tree a -> a
treeSum (Empty) = 0
treeSum (Node x left right) = x + treeSum left + treeSum right 
                               
contains :: Eq a => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node y left right) = x==y || contains x left && contains x right

leftMost :: Ord a => Tree a -> Maybe a 
leftMost (Empty) = Nothing
leftMost (Node x Empty _) = Just x
leftMost (Node _ left _) = leftMost left


data Step = StepL | StepR deriving (Show , Eq)

walk :: [ Step ] -> Tree a -> Maybe a
walk steps (Empty) = Nothing
walk (x:xs) (Node n left right) 
  | x==StepL  = walk xs left
  | x==StepR  = walk xs right
  | otherwise = Just n

data Expr = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Var String
  

eval :: Expr -> [( String , Integer ) ] -> Integer
eval (Num a)   _ = a
eval (Add a b) env = eval a env + eval b env
eval (Mul a b) env = eval a env * eval b env 
eval (Sub a b) env = eval a env - eval b env
eval (Div a b) env = eval a env `div` eval b env
eval (Var x) env = case lookup x env of
    Just value -> value
    Nothing -> error ("Variable not in scope")

    