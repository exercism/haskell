module StateOfTicTacToe (gameState, GameError(..), GameState(..)) where

data GameError = WrongTurnO | WrongTurnX | KeptPlaying deriving (Eq, Show)
data GameState = Win | Draw | List | Ongoing | Error GameError deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board = error "You need to implement this function."
