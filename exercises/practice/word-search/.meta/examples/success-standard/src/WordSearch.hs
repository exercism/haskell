module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (search' grid) wordList

search' :: [String] -> String -> (String, Maybe WordPos)
search' grid word = (word, listToMaybe hits)
    where
        lastRow = length grid - 1
        lastCol = length (head grid) - 1
        lastIdx = length word - 1
        hits = [WordPos{start=CharPos{col=startCol+1, row=startRow+1}, end=CharPos{col=endCol+1, row=endRow+1}} |
                    startCol <- [0 .. lastCol],
                    dirCol <- [- 1, 0, 1],
                    let endCol = startCol + lastIdx*dirCol,
                    endCol >= 0 && endCol <= lastCol,
                    startRow <- [0 .. lastRow],
                    dirRow <- [- 1, 0, 1],
                    let endRow = startRow + lastIdx*dirRow,
                    endRow >= 0 && endRow <= lastRow,
                    let gridWord = [
                                        (grid!!r)!!c | idx <- [0 .. lastIdx],
                                        let c = startCol + idx*dirCol,
                                        let r = startRow + idx*dirRow
                                   ],
                    and $ zipWith (==) word gridWord
               ]
