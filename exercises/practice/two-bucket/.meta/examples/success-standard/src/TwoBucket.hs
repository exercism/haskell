module TwoBucket (measure) where

import Control.Monad.State ( evalState, MonadState(put, get), StateT )
import qualified Control.Monad.Identity as Data.Functor.Identity
import Data.Maybe (mapMaybe)

-- Current status of the buckets, or their initial capacity
type Buckets = (Int, Int)
-- Sequence of moves
type Moves = [Buckets]
-- Solution candidates
type Candidates = [Moves]

measure :: Buckets -> Int -> Maybe (Int, Buckets)
measure capacity target =
    if null solution
    then Nothing
    else let sol = head solution in Just (length sol - 1, head sol)
    where
        solution = evalState (solve capacity target) [[(0, 0)]]

solve :: Buckets -> Int -> StateT Candidates Data.Functor.Identity.Identity Candidates
solve (c1, c2) target = solve'
    where
        solve' = do
            oldCandidates <- get
            if any isTarget oldCandidates
            then return $ filter isTarget oldCandidates
            else do
                let newCandidates = oldCandidates >>= expand
                if null newCandidates
                then return []
                else do
                    put newCandidates
                    solve'
        isTarget []           = False
        isTarget ((s1, s2):_) = s1 == target || s2 == target

        expand :: Moves -> Candidates
        expand candidate = allMoves (head candidate) >>= prependMove candidate

        allMoves :: Buckets -> Moves
        allMoves buckets = mapMaybe ($ buckets) [move12, move21, empty1, empty2, fill1, fill2]

        prependMove :: Moves -> Buckets -> [Moves]
        prependMove moves buckets = [buckets:moves | buckets `notElem` moves]

        -- Pour 1st bucket into the 2nd one
        move12 (s1, s2)
            | s1 == 0    = Nothing
            | s12 == c2  = Nothing
            | s12 < c2   = Just (0, s12)
            | otherwise  = Just (s12 - c2, c2)
            where
                s12 = s1 + s2

        -- Pour 2nd bucket into the 1st one
        move21 (s1, s2)
            | s2 == 0    = Nothing
            | s12 < c1   = Just (s12, 0)
            | otherwise  = Just (c1, s12 - c1)
            where
                s12 = s1 + s2

        -- Empty 1st bucket
        empty1 (s1, s2)
            | s1 == 0   = Nothing
            | s2 == c2  = Nothing
            | otherwise = Just (0, s2)

        -- Empty 2nd bucket
        empty2 (s1, s2)
            | s2 == 0   = Nothing
            | otherwise = Just (s1, 0)

        -- Fill 1st bucket
        fill1 (s1, s2)
            | s1 == c1   = Nothing
            | otherwise = Just (c1, s2)

        -- Fill 2nd bucket
        fill2 (s1, s2)
            | s2 == c2  = Nothing
            | s1 == 0   = Nothing
            | otherwise = Just (s1, c2)
