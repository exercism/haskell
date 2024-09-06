module Grep (grep, Flag(..)) where

data Flag = N | L | I | V | X deriving (Eq, Ord)

type Flags = [Flag]
type File = (String, [String])
type Files = [File]

grep :: String -> Flags -> Files -> [String]
grep pattern flags files = error "You need to implement this function."
