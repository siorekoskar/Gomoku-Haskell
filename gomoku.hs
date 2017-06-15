import Data.List
import Data.Maybe

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

data Color = Black | White | Empty deriving Eq

chainVal 2 = 1
chainVal 3 = 5
chainVal 4 = 100
chainVal x = 0

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

instance Show Point where
    show (Point x y color) = show color

instance Show Color where
    show Empty = " _ "
    show Black = " o "
    show White = " x "

-- getPoints:: Board -> [[Point]]
-- getPoints _ _ cells = cells

-- evalRows:: [[Point]] -> Int
-- evalRows cells =

getRow::Int -> [[Int]] -> Int -> [Int]
getRow what cells which = elemIndices what (cells!!which)

evalRow:: Int -> [Int] -> Int -> Int -> Int
evalRow lastInd [] sumator rowCount = rowCount + (chainVal sumator)
evalRow lastInd (x:xs) sumator rowCount
    | lastInd == (-1) = evalRow x xs 1 0
    | x == (lastInd+1) = evalRow x xs (sumator+1) rowCount
    | x /= (lastInd+1) = evalRow x xs 1 (rowCount + (chainVal sumator))

evalRowStart:: (Eq a) => a -> [a] -> Int
evalRowStart what ls = evalRow (-1) (elemIndices what ls) 0 0

makeColumns :: Int -> Int -> [Point]-> [Point]
makeColumns x y list
    | y == -1 = list
--    | y == 9 && x == 9 = makeColumns x (y-1) ((Point x y Black):list) -- branch getposition
    | otherwise = makeColumns x (y-1) (b:list)
    where b = Point x y Empty

makeBoard :: Int -> Int-> [[Point]] -> [[Point]]
makeBoard x y list
    | x == -1 = list
    | otherwise = makeBoard (x-1) y (b:list)
    where b = makeColumns x y []

board :: Board
board = Board 19 (makeBoard 18 18 [])
newBoard = Board 19 (insertF board 5 8 Black)

insertFigure :: Board -> Int -> Int -> Color -> Board
insertFigure board x y figure
    | x > size board || y > size board || x < 0 || y < 0 = board
    | color (cells board !! x !! y) == Empty = Board 19 (insertF board x y figure)
    | color (cells board !! x !! y) == figure = board

insertF :: Board -> Int -> Int -> Color -> [[Point]]
insertF board x y figure =
     let (a,b) = splitAt x (cells board)
        in let d = [addPoint x y figure (head b)]
            in a ++ (d) ++ (tail b)

addPoint :: Int -> Int -> Color -> [Point] -> [Point]
addPoint x y figure list =
    let (a,b) = splitAt y list in (a ++ [Point x y figure] ++ (tail b))

main :: IO ()
main = do
    loop board

loop:: Board -> IO()
loop board1 = do
    putStrLn "Ruch gracza Kolko"
    putStr "Wiersz: "
    x <- getLine
    putStr "Kolumna: "
    y <- getLine
    let board2 = insertFigure board1 (read x::Int) (read y::Int) Black
    putStrLn $ show board2
    putStrLn "Ruch gracza Krzyzyk"
    putStr "Wiersz: "
    x <- getLine
    putStr "Kolumna: "
    y <- getLine
    let board1 = insertFigure board2 (read x::Int) (read y::Int) White
    putStrLn $ show board1
    loop board1
