module ETL (transform) where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)

type PointValue = Int
type UpperTiles = Text
type LowerTile = Char

transform :: Map PointValue UpperTiles -> Map LowerTile PointValue
transform = M.fromList . concatMap go . M.toList
  where go (v, tiles) = zip (T.unpack (T.toLower tiles)) (repeat v)
