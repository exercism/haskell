module TreeBuilding (Record(..), Tree, buildTree, node) where

data Record = Record { recordId :: Int, parentId :: Int } deriving (Eq, Show)

data Tree = Leaf Int | Branch Int [Tree] deriving (Eq, Show)

node :: Int -> [Tree] -> Tree
node i [] = Leaf i
node i children = Branch i children

buildTree :: [Record] -> Maybe Tree
buildTree records =
  let n = length records
      -- check every id from 0 to n - 1 appears exactly once with a valid parent
      ok i
        | i >= n = True
        | otherwise =
            case filter (\r -> recordId r == i) records of
              [r] ->
                if i == 0
                then parentId r == 0 && ok (i + 1)
                else parentId r < i && ok (i + 1)
              _ -> False
      -- find the children of p by looking at every id in turn
      childrenOf p i =
        if i >= n
        then []
        else case filter (\r -> recordId r == i) records of
               [r] ->
                 if parentId r == p && i /= p
                 then i : childrenOf p (i + 1)
                 else childrenOf p (i + 1)
               _ -> childrenOf p (i + 1)
      make i =
        let cs = childrenOf i 0
        in if null cs
           then Leaf i
           else Branch i (map make cs)
  in if n == 0
     then Nothing
     else if ok 0
          then Just (make 0)
          else Nothing
