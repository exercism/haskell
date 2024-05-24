module Grep (grep, Flag(..)) where

import Data.Char (toLower)
import Data.List (isInfixOf)

data Flag = N | L | I | V | X deriving (Eq, Ord)

type Flags = [Flag]
type File = (String, [String])
type Files = [File]

grep :: String -> Flags -> Files -> [String]
grep pattern flags files = concatMap grepInFile files
    where
        flagN = N `elem` flags
        flagL = L `elem` flags
        flagI = I `elem` flags
        flagV = V `elem` flags
        flagX = X `elem` flags
        pattern' = if flagI then map toLower pattern else pattern
        multiple = length files > 1
        grepInFile (fileName, content) = if flagL && not (null matchInFile) then [fileName] else matchInFile
            where
                matchInFile = concatMap grepInLine $ zip content [1..]
                grepInLine :: (String, Int) -> [String]
                grepInLine (line, lineNum) = [res | if flagV then not match else match]
                    where
                        line' = if flagI then map toLower line else line
                        match = if flagX then pattern' == line' else pattern' `isInfixOf` line'
                        res =
                            (if multiple then fileName <> ":" else "") <>
                            (if flagN then show lineNum <> ":" else "") <>
                            line
