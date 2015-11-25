
```bash
$ runhaskell -Wall bob_test.hs
```

## Making Your First Haskell Module

To create a module that can be loaded with `import Bob (responseFor)`, put this code in `Bob.hs`:

```haskell
module Bob (responseFor) where

responseFor :: String -> String
responseFor = undefined
```

## (Optional) HLint

HLint is a tool for suggesting possible improvements to Haskell code. These suggestions include ideas such as using alternative functions, simplifying code and spotting redundancies.

Installing HLint is easy:

```bash
$ cabal update
$ cabal install hlint
```

Cabal will place the executable in `~/.cabal/bin` on Linux, `%APPDATA%\cabal\bin` on Windows, or `~/Library/Haskell/bin` on OS X.  Once that directory is in your path, run `hlint Bob.hs` to get suggestions for your solution before you submit it.
