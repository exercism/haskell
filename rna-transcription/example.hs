module DNA (toRNA) where

toRNA :: String -> String
toRNA = map $ \c -> case c of
  'C' -> 'G'
  'G' -> 'C'
  'A' -> 'U'
  'T' -> 'A'
  _ -> error $ "Invalid DNA-base: " ++ show c
