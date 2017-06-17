import Gomoku2

-- ---------------------------------------------
pcAi :: IO ()
pcAi = do
    pcAiLoop board10

--pcAiLoop :: GameTree a -> IO ()
pcAiLoop board5 = do
    let color = Black
    putStr "Ruch gracza "
    putStrLn $ show color
    putStr "Wiersz: "
    x <- getLine
    putStr "Kolumna: "
    y <- getLine
    --let board5 = getFromTree boardTree
    let board2 = Board ((length $ (getPoints board5) !!1)) (insertF board5 (read x::Int) (read y::Int) color)
    putStrLn $ show board2
    putStr $ show color
    --putStr " posiada punktow: "
    let wynik = evalBoard board2 color
    checkResultPcAi board2 color wynik
    print wynik

checkResultPcAi:: Board -> Color -> Int -> IO()
checkResultPcAi board1 color wynik = do
    if(wynik >= 100000) then
        gameOver board1 color
    else do
        aiTime board1

aiTime board5 = do
        putStr "Ruch ai "
        let color = White
        let t1 = generateTree1 color board5
            --print t1
        let best1 = findBest t1
        let board2 = getFromTree best1
        putStrLn " "
        putStrLn $ show board2
        putStrLn $ show color
        let wynik = evalBoard board2 color
        check board2 wynik color

check board5 wynik color= do
    if (wynik >= 100000) then
        gameOver board5 color
    else do
        pcAiLoop board5

aiAi :: IO ()
aiAi = do
    aiAiLoop board10false Black

aiAiLoop board10 color = do
    putStr "Ai "
    putStr $ show color
    x <- getLine
    let t1 = generateTree1 color board10
    let best1 = findBest t1
    let board2 = getFromTree best1
    let wynik1 = getResult best1
    putStrLn $ show board2
    putStrLn "-------"
    checkResultAi board2 color wynik1


checkResultAi:: Board -> Color -> Int -> IO()
checkResultAi board1 color wynik = do
    if(wynik >= 100000) then
        gameOver board1 color
    else do
        if (color == Black) then do
            let color2 = White
            aiAiLoop board1 color2
        else do
            let color2 = Black
            aiAiLoop board1 color2

-------------------------------------------

main :: IO ()
main = do
    loop board White


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
