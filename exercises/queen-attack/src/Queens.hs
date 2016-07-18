module Queens (boardString, canAttack) where

-- Positions are specified as two-tuples.
-- The first element is is the column (file in Chess terms).
-- The second element is the row (rank in Chess terms).
-- (0, 0) is the top left of the board, and (7, 7) is the bottom right.

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = undefined

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack = undefined
