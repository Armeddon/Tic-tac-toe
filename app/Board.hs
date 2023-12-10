module Board(
    Player(..),
    TileType(..),
    Tile,
    Turn,
    Board,
    newTurn, 
    newBoard, 
    tilesFromBoard,
    winCheck, 
    drawCheck,
    execTurn,
    EndGame(..)
) where

import Data.List(foldl', find)
import Data.Maybe(isJust, fromJust)

data EndGame = WinX | WinO | Draw deriving Eq

instance Show EndGame where
    show end = case end of
        WinX -> "Player X wins!"
        WinO -> "Player O wins!"
        Draw -> "The game ends in a draw!"

data Player = PlayerX | PlayerO deriving (Show, Eq)

data TileType = TileEmpty | TileX | TileO deriving Eq

instance Show TileType where
    show tile'type = case tile'type of
        TileEmpty -> "."
        TileX -> "X"
        TileO -> "O"

data Tile = Tile {
    tile :: TileType,
    x_tile :: Int,
    y_tile :: Int
} deriving Eq

instance Show Tile where
    show tl = show $ tile tl

data Turn = Turn {
    player :: Player,
    x_turn :: Int,
    y_turn :: Int
} deriving (Show, Eq)

data Board = Board {
    tiles :: [[Tile]],
    size :: Int
} deriving (Show, Eq)

newTurn :: Player -> (Int, Int) -> Turn
newTurn plr (x, y) = Turn {
    player = plr,
    x_turn = x,
    y_turn = y
}

newBoard :: Int -> Board
newBoard n = Board {
    tiles = [[Tile { tile = TileEmpty, x_tile = i, y_tile = j } | i <- [0..(n-1)]] | j <- [0..(n-1)]],
    size = n
}

tilesFromBoard :: Board -> [[Tile]]
tilesFromBoard brd = tiles brd

getTile :: Board -> (Int, Int) -> TileType
getTile brd (x, y) = tile $ fromJust $ fromJust $
    find isJust $ map (\line -> find (\tile -> x_tile tile == x && y_tile tile == y) line) $ tiles brd

listEq :: Eq a => [a] -> Bool
listEq lst = all id $  zipWith (==) lst (tail lst)

condition :: (a -> Bool) -> a -> Maybe a
condition p x = if p x then Just x else Nothing

winCheck :: Board -> Maybe Player
winCheck brd = case find (isJust) $ 
    [condition listEq [getTile brd (i-1, j-1) | i <- [1..(size brd)]] | j <- [1..(size brd)]] ++
    [condition listEq [getTile brd (i-1, j-1) | j <- [1..(size brd)]] | i <- [1..(size brd)]] ++
    [condition listEq [getTile brd (i-1, i-1) | i <- [1..(size brd)]]] ++
    [condition listEq [getTile brd (i-1, size brd - i) | i <- [1..(size brd)]]]
    of
        Just (Just tiles) -> case head tiles of
            TileX -> Just PlayerX
            TileO -> Just PlayerO
            _ -> Nothing
        _ -> Nothing

drawCheck :: Board -> Bool
drawCheck brd = all id $ map (all (\tl -> tile tl /= TileEmpty)) $ tiles brd

execTurn :: Board -> Turn -> Maybe Board
execTurn brd trn = if getTile brd (x_turn trn, y_turn trn) == TileEmpty
    then Just $ Board {
        tiles = (map . map) (\tile -> if x_tile tile == x_turn trn && y_tile tile == y_turn trn
            then Tile { x_tile = x_tile tile, y_tile = y_tile tile, tile = case player trn of
                PlayerX -> TileX
                PlayerO -> TileO
            } else tile) $ tiles brd,
        size = size brd
    } else Nothing
