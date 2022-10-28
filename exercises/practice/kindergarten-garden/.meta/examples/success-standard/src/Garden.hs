module Garden (garden, lookupPlants, Plant(..)) where
import qualified Data.Map as M
import Data.List (sort)
import Data.List.Split (chunksOf)

data Plant = Grass
           | Clover
           | Radishes
           | Violets
           deriving (Enum, Eq, Show, Read)

type Student = String
type Garden = M.Map String [Plant]

fromChar :: Char -> Plant
fromChar c = case c of
  'G' -> Grass
  'C' -> Clover
  'R' -> Radishes
  'V' -> Violets
  _   -> error ("Unknown plant " ++ show c)

garden :: [Student] -> String -> Garden
garden students plantString = M.fromListWith (++) pairs
  where plantRows = map (map fromChar) (lines plantString)
        doRow = zip (sort students) . chunksOf 2
        pairs = concatMap doRow (reverse plantRows)

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants = M.findWithDefault []
