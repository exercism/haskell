# About

The boolean type `Bool` is an enumeration of `True` and `False`. The basic boolean operators are `&&` (and), `||` (or), and `not`.

```haskell
True || False           -- True
True && False           -- False
```

The three boolean operators each have a different operator precedence. They are evaluated in this order: `not` first, `&&` second, and finally `||`. If you want to bypass these rules, you can enclose a boolean expression in parentheses (`()`), as the parentheses have an even higher operator precedence.

```haskell
not True && False       -- False
not (True && False)     -- True
```