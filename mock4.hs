import Text.Read (Lexeme(String))
import Data.Binary.Get (isEmpty)
data Color  = Red | Green | Blue | Purple | Tirkuaz
colorToValue :: Color -> Int
colorToValue c = case c of
    Red   -> 1
    Blue  -> 2
    Green -> 3
    _     -> 0


data Film = Film String String | FilmWithYear String String String

film1 = Film "Star-Wars" "George Lucas"
film2 = FilmWithYear "The Avengers" "Stan Lee" "2011"

showFilm :: Film -> String
showFilm (Film f a) = f ++ " " ++ a 
showFilm (FilmWithYear f a y) = f ++ " " ++ a ++ " " ++ y

-- data Person = Person String String Int Float String String 

data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , height      :: Float 
                     , phoneNumber :: String
                     , ssn         :: String
                     }

fullName :: Person -> String
fullName person = firstName person ++ " " ++ lastName person

han = Person "Han" "Solo" 43 1.80 "526-2928" "1622XZ0"

lookup1 :: Eq a => a -> [(a,b)] -> Maybe b 
lookup1 _ [] = Nothing 
lookup1 key ((x,y):xs)
   | key == x = Just y
   | otherwise = lookup1 key xs


-- data Point = Point 
--    {x :: Float, y :: Float}
--    deriving (Show, Eq)

-- Lab Starts here

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
   deriving (Show, Eq)

nextDay d = case d of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Monday

safeHead :: [a] -> Maybe a 
safeHead []     = Nothing 
safeHead (x:xs) = Just x

keepValues :: [Maybe a] -> [a]
keepValues [] = []
keepValues (x:xs) = case x of 
  Just x  -> x : keepValues xs
  Nothing -> keepValues xs
                  
data Date = Date { year  :: Int
                 , month :: Int
                 , day   :: Int
                 }                  

differenceInYears :: Date -> Date -> Int
differenceInYears y1 y2 = year y1 - year y2   

data Point = Point Double Double 

euclideanDistance (Point x1 y1) (Point x2 y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

data ContactMethod 
    = Email String 
    | SMS String 
    | Letter String String String
    deriving (Show)

showContactMethod :: ContactMethod -> String
showContactMethod (Email e) = "Email: " ++ e
showContactMethod (SMS s) = "SMS: " ++ s
showContactMethod (Letter f l n) = "Letter: " ++ f ++ " " ++ l ++ " " ++ n

data Employee = Employee { employee_id :: Int
                         , salary      :: Double
                         , role        :: String
                         , phone       :: String
                         } deriving (Show)

idMap :: [Employee] -> [(String, Employee)]
idMap [] = []
idMap (x:xs) = (show (employee_id x), x) : idMap xs      

-- let emp = Employee { employee_id = 101, salary = 60000, role = "Dev", phone = "123456" }

sumAllS :: [Employee] -> Double 
sumAllS [] = 0
sumAllS (x:xs) = salary x + sumAllS xs 

averageSalary :: [Employee] -> Double
averageSalary employees = sumAllS employees / fromIntegral (length employees)


data Stack a = Stack [a]

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack a) = null a 

push :: a -> Stack a -> Stack a
push a (Stack b) = Stack (a:b)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x,Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x
