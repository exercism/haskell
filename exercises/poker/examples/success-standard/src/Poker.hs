module Poker (bestHands) where

import Data.Maybe (fromJust)
import Data.List  (isPrefixOf, nub, elemIndex, sortBy)

validHand :: String -> Bool
validHand h = and [ length (words h) == 5
                  , all (`elem` "A23456789TJQK") (head <$> h')
                  , all (`elem` "HSDC") (last <$> h')
                  , all ((==2) . length) h']
  where
    h' = parseHand h

parseHand :: String -> [String]
parseHand = map f . words
  where
    f x | "10" `isPrefixOf` x = 'T' : [last x]
        | otherwise           = x

rankHand :: String -> (Int, [Int])
rankHand hand | counts == [5]       = (9, ranks') 
              | straight && flush   = (8, ranks') 
              | counts == [4,1]     = (7, ranks') 
              | counts == [3,2]     = (6, ranks') 
              | flush               = (5, ranks') 
              | straight            = (4, ranks') 
              | counts == [3,1,1]   = (3, ranks') 
              | counts == [2,2,1]   = (2, ranks') 
              | counts == [2,1,1,1] = (1, ranks') 
              | otherwise           = (0, ranks') 
  where
    h        = parseHand hand
    r        = fromJust . flip elemIndex "..23456789TJQKA" . head <$> h
    groups   = let x = nub r in sortBy (flip compare) (zip (times r <$> x) x)
    counts   = fst <$> groups
    ranks    = snd <$> groups
    ranks'   = if ranks == [14,5,4,3,2] then [5,4,3,2,1] else ranks
    straight = length counts == 5 && (maximum ranks' - minimum ranks') == 4
    flush    = length (nub (last <$> words hand)) == 1
    times xs x = length $ filter (==x) xs

bestHands :: [String] -> Maybe [String]
bestHands hands | not (all validHand hands) = Nothing
                | otherwise                 = Just $ f (0,[]) [] hands
  where
    f _ r []                          = r
    f m r (x:xs) | null r || rank > m = f rank [x] xs
                 | rank == m          = f m (x : r) xs
                 | otherwise          = f m r xs
      where
        rank = rankHand x
