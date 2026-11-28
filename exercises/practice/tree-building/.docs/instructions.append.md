# Instructions append

## The stub

`src/TreeBuilding.hs` already contains a working solution that passes all the tests.
It is slow and hard to read; your task is to make it better without breaking the tests.

`buildTree` returns `Nothing` when the records do not form a valid tree.

## Changing the representation

The tests never use the constructors of `Tree` directly.
Instead they build expected trees with the function

```haskell
node :: Int -> [Tree] -> Tree
```

where `node i children` is the node with id `i` and the given children, in ascending order of id.

This means you are free to change how `Tree` is represented, for example to

```haskell
import qualified Data.Tree as T

type Tree = T.Tree Int

node :: Int -> [Tree] -> Tree
node = T.Node
```

as long as `Tree` still has `Eq` and `Show` instances and `node` keeps its meaning.
If you use a library such as `containers`, remember to add it to the `dependencies` in `package.yaml`.
