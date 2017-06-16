import Data.List

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

instance Eq Point where
    (Point _ _ c1) == (Point _ _ c2) = c1 == c2

data Color = Black | White | Empty deriving Eq

data GameTree a = Node a [GameTree a] | EmptyTree deriving Show

board :: Board
board = Board 19 (makeBoard 18 18 [])
newBoard = Board 19 (insertF board 5 8 Black)
newBoard2 = Board 19 (insertF newBoard 6 9 Black)
board5 = Board 4 (makeBoard 3 3 [])
board5N = Board 4 (insertF board5 3 3 White)

genBoardsStart:: Board -> Color -> [[Board]]
genBoardsStart board1 color = genBoards board1 [] color ((length $ (getPoints board1)!!1)-1)
--uzywac genBoardsStart board Black

genBoards:: Board -> [[Board]]-> Color -> Int -> [[Board]]
genBoards _ xs _ (-1) = xs
genBoards board1 xs color which = genBoards board1 (boardNew:xs) color (which-1)
    where boardNew = (generateBoards board1 color which ((length $ (getPoints board1)!!1)-1) [])

generateBoards:: Board -> Color -> Int -> Int -> [Board] -> [Board]
generateBoards _ _ _ (-1) xs = xs
generateBoards board1 color which j xs
    | elemIndices j available /= [] = generateBoards board1 color which (j-1) ((insertFigure board1 which j color):xs)
    | otherwise = generateBoards board1 color which (j-1) (board1:xs)
    where available = ((getAvailablePositions board1)!!which)

getAvailablePos:: Board -> Int -> [Int]
getAvailablePos (Board len cells) which = [x | x <- (elemIndices (Point 1 1 Empty) (cells!!which))]

getAvailablePositions:: Board -> [[Int]]
getAvailablePositions (Board len cells) = [x | j <- [0..(len-1)], x <- [getAvailablePos board j]]

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

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

getPoints:: Board -> [[Point]]
getPoints (Board _ cells) = cells

getRow::(Eq a) => a -> [[a]] -> Int -> [Int]
getRow what cells which = elemIndices what (cells!!which)

evalRow:: Int -> [Int] -> Int -> Int -> Int
evalRow lastInd [] sumator rowCount
    | sumator == 5 = (-1)
    | otherwise = rowCount + (chainVal sumator)
evalRow lastInd (x:xs) sumator rowCount
    | sumator == 5 = (-1)
    | lastInd == (-1) = evalRow x xs 1 0
    | x == (lastInd+1) = evalRow x xs (sumator+1) rowCount
    | x /= (lastInd+1) = evalRow x xs 1 (rowCount + (chainVal sumator))

evalRowStart:: (Eq a) => a -> [a] -> Int
evalRowStart what ls = evalRow (-1) (elemIndices what ls) 0 0

evalRows:: (Eq a) => a -> [[a]] -> Int
evalRows color ls = sum [x | j <- [0..((length ls)-1)], x <- [(evalRowStart color (ls!!j))]]

evalDiags:: (Eq a) => a -> [[a]] -> Int -> Int
evalDiags color [] sumator = sumator
evalDiags color (x:xs) sumator = evalDiags color xs (sumator + evalRowStart color x)

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

insertFigure :: Board -> Int -> Int -> Color -> Board
insertFigure board x y figure
    | color (cells board !! x !! y) /= Empty = board
    | x > size board || y > size board || x < 0 || y < 0 = board
    | color (cells board !! x !! y) == Empty = Board ((length $ (getPoints board)!!1)-1) (insertF board x y figure)
    | color (cells board !! x !! y) == figure = board

insertF :: Board -> Int -> Int -> Color -> [[Point]]
insertF board x y figure =
     let (a,b) = splitAt x (cells board)
        in let d = [addPoint x y figure (head b)]
            in a ++ (d) ++ (tail b)

addPoint :: Int -> Int -> Color -> [Point] -> [Point]
addPoint x y figure list =
    let (a,b) = splitAt y list in (a ++ [Point x y figure] ++ (tail b))


rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

main :: IO ()
main = do
    loop board White

evalBoard:: Board -> Color -> Int
evalBoard board1 color
    | wynik1 /= (-1) && wynik2 /= (-1) && wynik3 /= (-1) && wynik4 /= (-1) = wynik1+wynik2+wynik3+wynik4
    | otherwise = (-1)
    where
        wynik1 = evalRows (Point 1 1 color) (getPoints board1)
        wynik2 = evalRows (Point 1 1 color) (rotateRight $ getPoints board1)
        wynik3 = evalDiags (Point 1 1 color) (diagonals $ getPoints board1) 0
        wynik4 = evalDiags (Point 1 1 color) (diagonals $ rotateRight $ getPoints board1) 0

loop:: Board -> Color -> IO()
loop board1 color = do
    putStr "Ruch gracza "
    putStrLn $ show color
    putStr "Wiersz: "
    x <- getLine
    putStr "Kolumna: "
    y <- getLine
    let board2 = insertFigure board1 (read x::Int) (read y::Int) color
    putStrLn $ show board2
    putStr $ show color
    putStr " posiada punktow: "
    let wynik = evalBoard board2 color
    print wynik
    checkResult board2 color wynik

checkResult:: Board -> Color -> Int -> IO()
checkResult board1 color wynik = do
    if(wynik == (-1)) then
        gameOver board1 color
    else do
        if (color == Black) then do
            let color2 = White
            loop board1 color2
        else do
            let color2 = Black
            loop board1 color2

gameOver:: Board -> Color -> IO()
gameOver board1 color = do
    putStrLn $ show board1
    putStrLn $ show color
    putStrLn " WYGRAL"
