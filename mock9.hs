-- -- | A Row is a list of integers
-- type Row = [Int]
-- -- | A Board is a list of rows
-- type Board = [Row]

-- -- | Returns an empty square board of the given size n x n.
-- emptyBoard :: Int -> Board
-- emptyBoard n = replicate n (emptyRow n)
--   where emptyRow n = replicate n 0

-- -- merge (n : m : ns) 
-- --   | n == m = 2 * n : merge ns
-- --   | otherwise = n : merge (m : ns)
-- -- merge x = x  

-- -- row' = merge (filter (/= 0) Row)
-- -- zeros = replicate (length Row - length row') 0

-- moveLeft :: Row -> Row
-- moveLeft row = row' ++ zeros
--   where merge (n : m : ns)
--          | n == m = 2 * n : merge ns
--          | otherwise = n : merge (m : ns)
--         merge x = x
--         row' = merge (filter (/= 0) row)
--         zeros = replicate (length row - length row') 0


-- moveRight :: Row -> Row
-- moveRight = reverse . moveLeft . reverse

-- moveLeftBoard board = map moveLeft board
-- moveRightBoard board = map moveRight board

-- transpose :: [[a]] -> [[a]]
-- transpose ([]:_) = []
-- transpose xss = (map head xss) : transpose (map tail xss)

-- data Direction = U | D | L | R

-- move :: Board -> Direction -> Board
-- move board dir = case dir of
--   R -> map moveRight board
--   L -> map moveLeft board
--   U -> transpose $ move (transpose board) L
--   D -> transpose $ move (transpose board) R
  

-- boardFull :: Board -> Bool
-- boardFull = all (notElem 0)

-- noMergeInRow row = all (\(a, b) -> a /= b) (zip row (tail row))

-- noMergeInBoard :: Board -> Bool
-- noMergeInBoard board =
--   all noMergeInRow board &&
--   all noMergeInRow (transpose board)

-- noMovesLeft :: Board -> Bool
-- noMovesLeft board = boardFull board && noMergeInBoard board


-- fullBoards :: [Board]
-- fullBoards = [
--    [[4,8,32,64],
--    [128,256,16,4],
--    [4,16,1024,2],
--    [2,8,2,4]],
--    [[2048,16,128,64],
--    [2,256,16,512],
--    [4,32,1024,2],
--    [2,8,2,8]]
--    ] 

-- testFullBoards :: Bool
-- testFullBoards = all noMovesLeft fullBoards

-- replaceAtIndex :: Int -> a -> [a] -> [a]
-- replaceAtIndex _ _ [] = error "replaceAtIndex: Index out of bounds"
-- replaceAtIndex n newVal (x:xs)
--   | n == 0 = newVal : xs
--   | otherwise = x : replaceAtIndex (n - 1) newVal xs

-- play2048 :: IO ()
-- play2048 = do
--   let newboard = emptyBoard 4
--   gameLoop newboard True

-- gameLoop :: Board -> Bool -> IO ()
-- gameLoop board needNewTile = do
--   prettyPrint board
--   putStrLn "--------"
--   if noMovesLeft board
--     then do
--       let maxscore = maxTile board
--       putStrLn "Game Over"
--       putStrLn $ "Your max score is " ++ show maxscore
--     else do
--       board' <- addTileToBoard board needNewTile
--       prettyPrint board'
--       putStrLn "--------"
--       putStrLn "Type U, D, L, R to move"
--       -- This part can be improved to be more flexible on input and not crash unless the input is U,D,L or R
--       direction <- readLn :: IO Direction
--       let board'' = move board' direction
--       let needNewTile = board' /= board''
--       gameLoop board'' needNewTile

-- addTileToBoard :: Board -> Bool -> IO Board
-- addTileToBoard board needNewTile
--   | needNewTile = return board
--   | otherwise = do
--       let freeSquares = positionsOfFreeSquares board
--       randomPos <- selectRandom freeSquares
--       tileValue <- generateTile
--       let board' = placeValueIn tileValue board randomPos
--       return board'

-- selectRandom :: [a] -> IO a
-- selectRandom xs = do
--   index <- randomRIO (0, length xs - 1)
--   return (xs !! index)

-- generateTile :: IO Int
-- generateTile = do
--   randomNumber <- randomRIO (0::Int, 99::Int)
--   if randomNumber < 10 -- we want about 10% of the time to get a 4
--     then return 4
--     else return 2

-- ------------- Formatting the board -------------------------------------------------------
-- prettyPrint :: Board -> IO ()
-- prettyPrint board = mapM_ putStrLn (formatRows board)

-- formatRow :: Board -> [Int] -> String
-- formatRow board row = unwords (zipWith (curry padElement) row maxColumnWidths)
--     where
--         maxColumnWidths = map (maximum . map (length . show)) (transpose board)
--         padElement (n, maxWidth) = replicate (maxWidth - length (show n)) ' ' ++ show n

-- formatRows :: Board -> [String]
-- formatRows board = map (formatRow board) board

-- Lab starts here

concatPairs :: [(a,a)] -> [a]
concatPairs = foldr (\(x, y) acc -> x:y:acc ) []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x acc-> if x`elem`acc then acc else x:acc ) []

f1 s = zip list (map length list) where list = removeDuplicates (words s)

averagePairs :: Fractional a => [(a,a)] -> (a,a)
averagePairs li   = (average x, average y) 
      where 
        (x,y)     = unzip li
        average l = sum l / fromIntegral (length l) 

data Sudoku = Sudoku { rows :: [[ Maybe Int ]] }
  deriving (Show , Eq)        

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

isSudokuRow :: [Maybe Int] -> Bool 
isSudokuRow x = length x == 9 && all isValid x
    where 
        isValid (Just n) = n>=1&&n<=9
        isValid Nothing  = True

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs ) = length rs==9 && all isSudokuRow rs         

noBlanks :: Sudoku -> Bool
noBlanks (Sudoku rs) = all (all isFull) rs
    where
        isFull (Just _) = True
        isFull Nothing  = False 

printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = mapM_ printRow rs
  where 
    printRow :: [Maybe Int] -> IO()        
    printRow r = putStrLn $ concatMap showCell r

    showCell :: Maybe Int -> String
    showCell (Just n) = show n
    showCell Nothing  = "."

data PasswordRequirement = MinLength Int
                           | ContainChar [Char]
                           | NotContain  [Char]
                           | And PasswordRequirement PasswordRequirement
                           | Or PasswordRequirement PasswordRequirement 

special = "!@#$%^&*"
capital = ['A'..'Z']                        
passwordRule = And (MinLength 8)  (And (ContainChar special) (ContainChar capital))                     

lowerAndGreater :: (Show a, Ord a, Num a) => a -> [a] -> String
lowerAndGreater ref xs = 
  let (low, high) = foldr count (0, 0) xs
  in "Lower: " ++ show low ++ ", Greater: " ++ show high
  where
    count x (l, g)
      | x < ref   = (l + 1, g)
      | x > ref   = (l, g + 1)
      | otherwise = (l, g)  -- equal: don't count

elIndexes :: Eq a => a -> [a] -> [Int]
elIndexes el xs = [i | (x, i) <- zip xs [0..], x == el]
