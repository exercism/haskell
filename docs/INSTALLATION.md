## Using the Haskell Platform

Download and install a recent Haskell Platform (GHC) for your OS from [haskell.org/platform](http://www.haskell.org/platform/). Linux distributions are likely to name this package `haskell-platform`. If you're using Xcode 5, you may need to tweak your Haskell Platform installation. See [The Glasgow Haskell Compiler (GHC) on OS X 10.9 (Mavericks)](http://justtesting.org/post/64947952690/the-glasgow-haskell-compiler-ghc-on-os-x-10-9) for details.

### MacOS

```bash
$ brew install ghc cabal-install
$ cabal update
$ cabal install hunit
```

## Using Stack by itself

Follow the [installation instructions](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) for your operating system. After completing the install, test that you have installed it correctly by runnint `stack` from a new terminal instance. If you get a printout of the help documentation then you have installed it correctly.

Run `stack install HUnit QuickCheck attoparsec parallel random regex-base regex-compat split stm text vector` to install the global packages required to do the exercism exercises.

### Testing your exercise

Each of the exercises contains a file named `<exercise>_test.hs`. In that file will be an import statement that lets you know the file you are expected to create. For instance, in the Leap exercise:

```haskell
-- in the file leap_test.hs
import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
-- This is the file you are expected to create (LeapYear.hs)
import LeapYear (isLeapYear)

-- ...
```

Create the file in the same directory as the `<exercise>_test.hs`, in this case you would create `LeapYear.hs`.

In your terminal instance, change the active directory to the exercise folder, and run `stack runghc <exercise>_test.hs`. This will run the test file that imports your solution and run it against the tests.
