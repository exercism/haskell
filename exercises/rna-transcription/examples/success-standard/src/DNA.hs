module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'C' = return 'G'
    fromDNA 'G' = return 'C'
    fromDNA 'A' = return 'U'
    fromDNA 'T' = return 'A'
    fromDNA  c  = Left c
