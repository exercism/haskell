module ETL (transform) where
import Data.Char (toLower)
import qualified Data.Map as M

type PointValue = Int
type LowerTile = Char
type UpperTile = Char

transform :: M.Map PointValue [UpperTile] -> M.Map LowerTile PointValue
transform = M.fromList . concatMap go . M.toList
  where go (v, tiles) = zip (map toLower tiles) (repeat v)
