import Data.List

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

data Color = Black | White | Empty

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

instance Show Point where
    show (Point _ _ color) = show color

instance Show Color where
    show Empty = " _ "
    show Black = " o "
    show White = " x "

-- makeBoard :: Int -> [[Point]]
-- makeBoard a = replicate a $ replicate a (Point 0 0 Empty)

makeColumns :: Int -> Int -> [Point]-> [Point]
makeColumns x y list
    | y == 0 = list
    | otherwise = makeColumns x (y-1) (b:list)
    where b = Point x y Empty

makeBoard :: Int -> Int-> [[Point]] -> [[Point]]
makeBoard x y list
    | x == 0 = list
    | otherwise = makeBoard (x-1) y (b:list)
    where b = makeColumns x y []

board:: Board
board = Board 19 (makeBoard 19 19 [])
