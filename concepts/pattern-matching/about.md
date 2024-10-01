# About

When writing Haskell functions, we can make use of an [assertive style][assertive-style] with [pattern matching][pattern-match-doc]:

```haskell
read_file = do
  {:ok, contents} <- readFile "hello.txt"
  contents
```

- Pattern matching is explicitly performed using the match operator, [`=/2`][match-op].

  - Matches succeed when the _shape_ of the data on the left side of the operator matches the right side.
  - When matches succeed, variables on the left are bound to the values on the right.
  - Using an underscore, `_` (also know as wildcard), allows us to disregard the values in those places.

    ```haskell
    (True, number, _) <- (True, 5, [4.5, 6.3])
    number
    -- -> 5 is bound to this variable
    ```

- Pattern matches may also occur in a function clause head, so that only arguments that match the pattern will invoke the function.
- Variables can be bound in a function clause pattern match.

  ```haskell
  named_function :: Bool -> (Bool, Int)
  named_function True = (True, 1)

  named_function(True)
  -- -> (True, 1)
  -- The first function clause matches, so it is invoked

  named_function(False)
  -- (FunctionClauseError) Non-exhaustive patterns in function named_function
  ```

[assertive-style]: https://blog.plataformatec.com.br/2014/09/writing-assertive-code-with-elixir/
[pattern-match-doc]: https://hexdocs.pm/elixir/pattern-matching.html
[match-op]: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#=/2
[getting-started-pin-operator]: https://hexdocs.pm/elixir/pattern-matching.html#the-pin-operator
