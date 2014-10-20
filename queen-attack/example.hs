module Queens (boardString, canAttack) where

type Position = (Int, Int)

boardString :: Maybe Position -> Maybe Position -> String
boardString w b = concatMap rowString [0..7]
  where
    rowString row = concatMap (tileString row) [0..7]
    tileString row col = [tileChar (row, col), tileSep col]
    tileSep 7 = '\n'
    tileSep _ = ' '
    tileChar pos
      | w == Just pos = 'W'
      | b == Just pos = 'B'
      | otherwise     = '_'

canAttack :: Position -> Position -> Bool
canAttack (wr, wc) (br, bc) = sameRow || sameCol || diagonal
  where
    dr = wr - br
    dc = wc - bc
    sameRow  = dr == 0
    sameCol  = dc == 0
    diagonal = abs dr == abs dc
