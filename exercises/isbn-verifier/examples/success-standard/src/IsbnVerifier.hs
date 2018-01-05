module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn "" = False
isbn xs = length nums == 10 && g nums `mod` 11 == 0 && l <= 1
  where
    nums = let nxs = f xs in if last xs == 'X' then nxs++[(1,10)] else nxs
    f :: String -> [(Int, Int)]
    f = zip [10,9..1] . map (\x -> read [x]) . filter (`elem` ['0'..'9'])
    g = sum . map (uncurry (*))
    l = length $ filter (=='X') xs
