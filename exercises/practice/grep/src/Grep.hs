module Grep (grep, Flag(..)) where

data Flag = N | L | I | V | X deriving (Eq, Ord)

type Flags = [Flag]

grep :: String -> Flags -> [FilePath] -> IO [String]
grep string flags files = error "You need to implement this function."
