module Main where

import Data.Maybe(isJust, fromJust)
import Text.Read(readMaybe)
import System.Exit(exitSuccess)

import Board

split :: Char -> String -> [String]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

getTurn :: Player -> IO Turn
getTurn player = do
    putStrLn $ show player ++ ": take your turn!"
    input <- getLine
    let nums = split ' ' input
    if length nums /= 2 || head input == ' ' || last input == ' ' then do
        putStrLn "Incorrect move format!"
        getTurn player
    else do
        let x = readMaybe $ nums !! 0 :: Maybe Int
        let y = readMaybe $ nums !! 1 :: Maybe Int
        if isJust x && isJust y then
            return $ newTurn player (fromJust x - 1, fromJust y - 1)
        else do
            putStrLn "Not a number!"
            getTurn player

printBoard :: Board -> IO ()
printBoard brd = mapM_ (\line ->
    mapM_ (\tile -> putStr $ show tile ++ " ") line >>
    putStrLn "") $ tilesFromBoard brd

getBoard :: IO Board
getBoard = do
    putStrLn "Choose board size: "
    input <- getLine
    let x = readMaybe input :: Maybe Int
    case x of
        Just n -> return $ newBoard n
        Nothing -> do
            putStrLn "Bad format!"
            getBoard

makeTurn :: Board -> Player -> IO ()
makeTurn board player = if drawCheck board
    then endGame board Draw
    else do
        printBoard board
        turn <- getTurn player
        let new_board = execTurn board turn
        case new_board of
            Just brd -> do
                if isJust $ winCheck brd
                then endGame brd $ case player of
                    PlayerX -> WinX
                    PlayerO -> WinO
                else makeTurn brd $ case player of
                    PlayerX -> PlayerO
                    PlayerO -> PlayerX
            Nothing -> do
               putStrLn "Can't move there"
               makeTurn board player

endGame :: Board -> EndGame -> IO ()
endGame board end = do
    printBoard board
    putStrLn $ show end
    exitSuccess

main :: IO ()
main = do
    board <- getBoard
    makeTurn board PlayerX
