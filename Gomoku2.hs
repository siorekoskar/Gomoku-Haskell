{-# LANGUAGE InstanceSigs #-}

module Gomoku2 where

import Data.List

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

instance Eq Point where
    (Point _ _ c1) == (Point _ _ c2) = c1 == c2

data Color = Black | White | Empty deriving Eq

data GameTree a = Node a Int Int Int [GameTree a] deriving Show

-- instance Functor GameTree a where
--     fmap:: (a->b) -> GameTree a -> GameTree a
--     fmap f (Node b v x y (xs:list)) = Node b v x y

board :: Board
board = Board 19 (makeBoard 18 18 [])
newBoard = Board 19 (insertF board 5 8 Black)
newBoard2 = Board 19 (insertF newBoard 6 9 Black)
board5 = Board 4 (makeBoard 3 3 [])
board5N = Board 4 (insertF board5 0 0 White)
board5B = Board 4 (insertF board5 0 0 Black)
board5D = Board 4 (insertF board5B 2 2 Black)
board5e = Board 4 (insertF board5D 2 3 White)
board5f = Board 4 (insertF board5e 3 3 White)
board5g = insertFigure board5 0 0 White
board10 = Board 10 (makeBoard 9 9 [])
board10false = Board 10 (insertF board10 5 5 White)
board10g = Board 10 (insertF board10 9 8 White)
board10f = Board 10 (insertF board10g 9 9 White)
board10false2 = Board 10 (insertF board10false 9 9 Black)

oppositeColor color
    | color == Black = White
    | otherwise = Black

--(evalBoard board color)

generateTree1 color board = Node board (evalBoard board color) 0 0 [(Node x (evalBoard x color) i j [])| i <- [0..len], j <- [0..len], x <- [((genBoardsStart board color)!!i!!j)]]
    where len = ((length $ (getPoints board)!!1)-1)

generateTree2 color board = Node board (evalBoard board color) 0 0 [(generateTree1 color x) | i <- [0..len], j<- [0..len], x <- [(genBoardsStart board color)!!i!!j]]
    where len =((length $ (getPoints board)!!1)-1)

generateTree3 color board = Node board (evalBoard board color) 0 0 [(generateTree2 color x) | i <- [0..len], j<- [0..len], x <- [(genBoardsStart board color)!!i!!j]]
    where len =((length $ (getPoints board)!!1)-1)

getFromTree (Node board _ _ _ _) = board

getResult (Node _ v _ _ _) = v

findBest (Node board v x y ((Node b v1 x1 y1 []):xs)) = getBestPos xs b v x y

getBestPos [] b v x y = (Node b v x y [])
getBestPos ((Node b v x y _):xs) bo v1 x1 y1
    | v1 <= v && (color /= (Point 1 1 Empty))  = getBestPos xs b v x y
    | otherwise = getBestPos xs bo v1 x1 y1
    where color = ((getPoints b)!!x!!y)

    -- | v1 <= v  = getBestPos xs b v x y
--    | v == (-1) = (Node b v x y [])
    -- | color /= (Point 1 1 Empty) && x == (length((getPoints b)!!1)-1) = getBestPos [] bo v1 x1 y1

    -- | color /= Empty = getBestPos xs bo v1 x1 y1

findWorst (Node board v x y ((Node b v1 x1 y1 []):xs)) = getWorstPos xs b v x y

getWorstPos [] b v x y = (Node b v x y [])
getWorstPos ((Node b v x y _):xs) bo v1 x1 y1
    | v1 >= v && (color /= (Point 1 1 Empty))  = getWorstPos xs b v x y
    | otherwise = getWorstPos xs bo v1 x1 y1
    where color = ((getPoints b)!!x!!y)

minFind1 board color = findWorst $ generateTree2 color board
maxFind1 board color = findBest


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
    | sumator == 5 = 100000
    -- | sumator == 5 = (-1)
    | otherwise = rowCount + (chainVal sumator)
evalRow lastInd (x:xs) sumator rowCount
    -- | sumator == 5 = (-1)
    | sumator == 5 = 100000
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
    | color (cells board !! x !! y) == Empty = Board ((length $ (getPoints board)!!1)) (insertF board x y figure)
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

evalBoard:: Board -> Color -> Int
evalBoard board1 color = wynik1+wynik2+wynik3+wynik4
--    | wynik1 /= (-1) && wynik2 /= (-1) && wynik3 /= (-1) && wynik4 /= (-1) = wynik1+wynik2+wynik3+wynik4
--    | otherwise = (-1)
    where
        wynik1 = evalRows (Point 1 1 color) (getPoints board1)
        wynik2 = evalRows (Point 1 1 color) (rotateRight $ getPoints board1)
        wynik3 = evalDiags (Point 1 1 color) (diagonals $ getPoints board1) 0
        wynik4 = evalDiags (Point 1 1 color) (diagonals $ rotateRight $ getPoints board1) 0
