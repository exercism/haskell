module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn "" = False
isbn xs = length nums == 10 && g nums `mod` 11 == 0 && l <= 1
  where
    nums = let nxs = f xs in if last xs == 'X' then nxs++[(1,10)] else nxs
    f :: String -> [(Int, Int)]
    -- for an input that isn't too long, only 10,9..1 should be used
    -- for an input that's too long, the 0 gets included,
    -- then the `length nums == 10` rejects it.
    f = zip [10,9..0] . map (\x -> read [x]) . filter (`elem` ['0'..'9'])
    g = sum . map (uncurry (*))
    l = length $ filter (=='X') xs
