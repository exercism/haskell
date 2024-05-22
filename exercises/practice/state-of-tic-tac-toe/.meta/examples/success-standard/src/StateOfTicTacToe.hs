module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | countO > countX          = Impossible
    | countX > countO + 1      = Impossible
    | winO && countX > countO  = Impossible
    | winX && countO >= countX = Impossible
    | winX                     = WinX
    | winO                     = WinO
    | countO + countX == 9     = Draw
    | otherwise                = Ongoing
    where
        transposedBoard = transpose board
        fallingDiag = zipWith (!!) board [0..]
        risingDiag = zipWith (!!) (reverse board) [0..]
        countX = count 'X'
        countO = count 'O'
        winX = win 'X'
        winO = win 'O'
        count ch = length $ filter (== ch) $ concat board
        win ch =
            any (all (== ch)) board ||
            any (all (== ch)) transposedBoard ||
            all (==ch) fallingDiag ||
            all (==ch) risingDiag
