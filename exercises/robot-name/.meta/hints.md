## Hints

To complete this exercise, you need to create the data type `Robot`,
as a mutable variable, and the data type `RunState`. You also need to
implement the following functions:

- `initialState`
- `mkRobot`
- `resetName`
- `robotName`

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym. To model state this exercise uses the
[`State`](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
monad. More specifically we combine the `State` monad with the `IO` monad using
the [`StateT` monad transfomers](http://book.realworldhaskell.org/read/monad-transformers.html).
All tests are run with `initialState` as the state fed into to `evalStateT`.
