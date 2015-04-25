## Making Your First Haskell Module

To create a module that can be loaded with `import Bob (responseFor)`, put this code in `Bob.hs`:

```haskell
module Bob (responseFor) where

responseFor :: String -> String
responseFor = undefined
```
