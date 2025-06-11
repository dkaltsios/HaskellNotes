-- main :: IO ()
-- main = do
--     putStrLn "Give three names?"
--     rs <- sequence [getLine, getLine, getLine]
--     print rs
--     numbers <- mapM (const (readLn :: IO Int)) [1..3]  -- Apply readLn to each element
--     putStrLn "You entered:"
--     print numbers
--     -- line <- getLine  
--     -- putStrLn $ "Hey " ++ line
--     -- putStr1 "Nitsas"
--     -- getTheDifference2
--     -- showTheDifference

 
-- charAction :: IO Char
-- charAction = do
--     c <- getChar
--     putChar c
--     return c

-- myGetLine :: IO String
-- myGetLine = do
--   x <- getChar
--   if x == '\n' then
--     return ""
--   else
--     do xs <- myGetLine 
--        return (x : xs)    

-- -- putChar :: Char -> IO ()

-- putStr1 :: String -> IO ()
-- putStr1 []     = return ()
-- putStr1 (x:xs) = do 
--     putChar x
--     putStr1 xs


-- -- putStr :: String -> IO ()
-- -- getLine :: IO String
-- -- writeFile :: FilePath -> String -> IO ()
-- -- readFile :: FilePath -> IO String
-- -- print :: Show a => a -> IO ()
-- -- type FilePath = String    


-- -- readLn :: Read a => IO a

-- showTheDifference :: IO ()
-- showTheDifference = do putStrLn "Enter two numbers:"
--                        x <- readLn
--                        y <- readLn
--                        putStr "The difference is: "
--                        print (x - y)

-- doTwice :: IO a -> IO (a, a)
-- doTwice io = do x <- io
--                 y <- io
--                 return (x, y)                       

-- getTheDifference2 :: IO Integer
-- getTheDifference2 = do putStrLn "Enter two numbers:"
--                        (x, y) <- doTwice readLn
--                        return (x - y)                

-- copyFile :: FilePath -> FilePath -> IO ()
-- copyFile fromFile toFile =
--   do c <- readFile fromFile
--      writeFile toFile c

-- For making calculations at input output
-- unsafeValue :: IO Int
-- unsafeValue = do
--   v1 <- mystery2 2 4
--   v2 <- mystery2 2 4
--   return (v1 + v2)

-- sequence $ map print [1,2,3]
-- Prints 
-- 1
-- 2
-- 3
-- [(),(),()]

-- sequence_ $ map print [1..3]
-- Prints
-- 1
-- 2
-- 3

-- mapM f = sequence . map f

-- Lab starts HERE

main :: IO ()
main = do 
       putStrLn ("Please enter your name: ")
       x <- getLine 
       putStrLn $ "Hello " ++ x

dont :: IO a -> IO ()
dont _ = return ()

mySequence :: [IO a] -> IO [a]
mySequence [] = return []
mySequence (x:xs) = do
    result <- x
    rest <- mySequence xs
    return (result : rest)


-- readChar :: IO Char -> Maybe a
readChar = do 
    a <- getChar
    case a of 
        ' ' -> return ()    
        _   -> putChar a 

numbers :: Int -> IO ()
numbers 0 = putStrLn "0"
numbers n = do
    putStrLn (show n) 
    numbers (n-1)

favoriteColor :: String -> IO Bool 
favoriteColor x = do
    putStrLn $ "Is this your favorite colour? " ++ x
    resp <- getChar
    return (resp == 'Y')

ioShow :: Int -> IO String 
ioShow x = return (show x) 

program :: IO ()
program = do
    putStrLn "Enter a sentence:" 
    sent <- getLine
    let revSent = unwords (reverse (words sent))
    putStrLn $ "The reversed sentence is: " ++ revSent
    
program1 :: IO ()
program1 = do
    putStrLn "Enter a temperature in Celcius:" 
    celcius <- readLn :: IO Double
    let fahrenheit = celcius * (9/5)+32
    putStrLn $ "The fahrenheit convergance comes out as: " ++ show fahrenheit

program2 :: IO ()
program2 = do
    putStrLn "Guess the number I am thinking" 
    guess <- readLn :: IO Int
    case guess of 
    if guess>4 then do
        putStrLn ("Too high")
        program2
    else if guess<4 then do
        putStrLn ("Too low")
        program2
    else do
        putStrLn ("Correct")    