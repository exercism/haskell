module StateOfTicTacToe (gameState, GameError(..), GameState(..)) where

import Data.List (transpose)

data GameError = WrongTurnO | WrongTurnX | KeptPlaying deriving (Eq, Show)
data GameState = Win | Draw | List | Ongoing | Error GameError deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | countO > countX          = Error WrongTurnO
    | countX > countO + 1      = Error WrongTurnX
    | winO && countX > countO  = Error KeptPlaying
    | winX && countO >= countX = Error KeptPlaying
    | winO || winX             = Win
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
