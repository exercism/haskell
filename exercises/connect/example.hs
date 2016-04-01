module Connect (resultFor, Color(..)) where

import Control.Arrow ((***), first, second)
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import Prelude hiding (lines)

data Color
  = White
  | Black
  deriving (Show,Eq)

type Board = A.Array (Int,Int) (Maybe Color)

-- Parse the lines into a board, assuming the list of lines is not empty
-- and the lines are of equal non-0 size.
parseLines :: [String] -> Board
parseLines [] = error "Empty lines"
parseLines ([]:_) = error "Empty first line"
parseLines lines@(firstLine:_) =
  let height = length lines
      width = length firstLine
  in A.array ((0,0),(width - 1,height - 1)) fieldAssocs
  where fieldAssocs :: [((Int,Int),Maybe Color)]
        fieldAssocs =
          do (l,y) <- zip lines [0 ..]
             (c,x) <- zip l [0 ..]
             let f = case c of
                     'O' -> Just White
                     'X' -> Just Black
                     _ -> Nothing
             return ((x,y),f)

-- Fields already seen during connection searching.
-- Has the same shape as the board. True means field seen.
type ConnArr s = STA.STArray s (Int,Int) Bool

emptyConnArr :: Board -> ST s (ConnArr s)
emptyConnArr b =
  MA.newArray (A.bounds b) False

neighbours :: Board -> (Int,Int) -> [(Int,Int)]
neighbours b c =
  let dirs =
        [first (+ 1)
        ,first (subtract 1)
        ,second (+ 1)
        ,second (subtract 1)
        ,subtract 1 *** (+ 1)
        ,(+ 1) *** subtract 1]
  in filter (A.inRange (A.bounds b)) . map ($ c) $ dirs

isTargetEdge :: Board -> Color -> (Int,Int) -> Bool
isTargetEdge b Black (x,_) =
  let (_,(ux,_)) = A.bounds b
  in x == ux
isTargetEdge b White (_,y) =
  let (_,(_,uy)) = A.bounds b
  in y == uy

startCoords :: Board -> Color -> [(Int,Int)]
startCoords b Black =
  let ((_,ly),(_,uy)) = A.bounds b
  in [(0,y) | y <- [ly .. uy]]
startCoords b White =
  let ((lx,_),(ux,_)) = A.bounds b
  in [(x,0) | x <- [lx .. ux]]

tryConnect :: Board -> Color -> ConnArr s -> (Int,Int) -> ST s Bool
tryConnect b color ca c =
  case b A.! c of
    Just fieldColor
      | fieldColor == color ->
        do seen <- MA.readArray ca c
           if seen
              then return False
              else if isTargetEdge b fieldColor c
                      then return True
                      else do MA.writeArray ca c True
                              or `fmap`
                                mapM (tryConnect b color ca)
                                     (neighbours b c)
    _ -> return False

resultFor :: [String] -> Maybe Color
resultFor lines =
  let board = parseLines lines
  in if resultFor' board Black
        then Just Black
        else if resultFor' board White
                then Just White
                else Nothing
  where resultFor' :: Board -> Color -> Bool
        resultFor' b color =
          runST $
          do ca <- emptyConnArr b
             or `fmap`
               mapM (tryConnect b color ca)
                    (startCoords b color)
