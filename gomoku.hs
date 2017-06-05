import Data.List

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

data Color = Black | White | Empty deriving Eq

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

instance Show Point where
    show (Point x y color) = show color

instance Show Color where
    show Empty = " _ "
    show Black = " o "
    show White = " x "

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
