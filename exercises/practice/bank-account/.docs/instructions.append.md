## Hints

To complete this exercise you need to implement the following functions:

- `openAccount` - Called at the start of each test. Returns a BankAccount.
- `closeAccount` - Called at the end of each test.
- `getBalance` - Get the balance of the bank account.
- `updateBalance` - Increment the balance of the bank account by the given amount.

The amount may be negative for a withdrawal.

The initial balance of the bank account should be 0.

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.

If you need help, here are some additional resources:

- Read about [concurrency](https://en.wikipedia.org/wiki/Concurrent_Haskell) in Haskell.
- Look into the [Software Transactional Memory](https://hackage.haskell.org/package/stm) package and its Transactional Variables.
