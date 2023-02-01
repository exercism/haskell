# Logical expression

```haskell
isLeapYear :: Integer -> Bool
isLeapYear year = divisibleBy 4 && (not (divisibleBy 100) || divisibleBy 400)
  where
    divisibleBy d = year `mod` d == 0
```

We can combine smaller logical statements into larger ones using the logical operators `&&` (and), `||` (or), and `not` (negation).


## Precedence

In school they teach you that `2 + 3 * 4` is to be read as meaning `2 + (3 * 4)`.
This is a convention, chosen for its convenience.
We say that the `*` operator has _higher [precedence][wikipedia-precedence]_ than `+`.

In logic similar ambiguities exist, and these are similarly resolved.
By contention &ndash; and so in Haskell &ndash; _and_ has higher precedence than _or_, and _not_ has higher precedence than both.

Concretely, `p || q && r` is to be read as `p || (q && r)`.

~~~~exercism/note
If you want to know the precedence of an operator, you can ask GHCi:

```haskell
ghci> :info &&
...
infixr 3 &&
ghci> :info +
...
infixl 6 +
ghci> :info not
...  -- no precedence information
```

Here, `infixr 3 &&` indicates that `&&` has precedence 3 and [associates][wikipedia-associativity] to the right.
Similarly, `infixl 6 +` means that `+` has precedence 6 and associates to the left.
No precedence is shown for `not`, because none has been explicitly defined.
However, `not` is just a regular function, and function application always has precedence 10.
~~~~

~~~~exercism/note
The solution highlighted above uses parentheses to group two tests together that would otherwise be grouped apart.
That is, this solution has the shape `P && (Q || R)`.
However, if the parentheses were removed the solution would still be correct!

Do you see why? Hint: fill out the [truth tables][wikipedia-truth-table] for both versions.

This is not possible in general: sometimes `(P && Q) || R` and `P && (Q || R)` disagree.

The motivation for the parentheses in the highlighted solution is efficiency; see below.
~~~~


## An example of laziness


Just like in many other languages, Haskell's logical operators display short-circuiting behavior:

```haskell
ghci> import Debug.Trace
ghci> -- `trace s x` prints the given string `s`
ghci> --   and immediately returns `x`.
ghci> cheap     x = trace "Cheap computation"     x
ghci> expensive x = trace "Expensive computation" x
ghci> cheap False && expensive True
Cheap computation
False
ghci> cheap True && expensive False
Cheap computation
Expensive computation
False
ghci> cheap True || expensive False
Cheap computation
True
ghci> cheap False || expensive False
Cheap computation
Expensive computation
False
```

Sometimes, the result is already completely determined by the left operand.
In such cases both `&&` and `||` avoid unnecessary work by skipping evaluation of the right operand.

So far, so usual.

However, in contrast to many other languages, Haskell does not have this behavior built in specifically for the logical operators.
In fact, these operators aren't built-in at all!
Instead they are [defined in the standard library][operators-source-code], as follows.

```haskell
(&&)        :: Bool -> Bool -> Bool
True  && x  =  x
False && _  =  False

(||)        :: Bool -> Bool -> Bool
True  || _  =  True
False || x  =  x
```

Both first pattern-match on their left operand to check whether it is `True` or `False`.
Next, depending on the match, they directly return the right operand or disregard it.

Because sometimes `&&` and `||` do not evaluate their second operand, we say that they are _[lazy][wikipedia-non-strict-evaluation] in their second argument_.

In the Leap solution highlighted above, laziness results in the following checks being evaluated.

| year | `divisibleBy 4` | `divisibleBy 100` | `divisibleBy 400` | is leap year |
| ---- | --------------- | ----------------- | ----------------- | ------------ |
| 2020 | `True`          | `False`           | not evaluated     | `True`       |
| 2019 | `False`         | not evaluated     | not evaluated     | `False`      |
| 2000 | `True`          | `True`            | `True`            | `True`       |
| 1900 | `True`          | `True`            | `False`           | `False`      |

Laziness of `&&` and `||` and clever grouping ensure that only minimal effort is spent:

- 75% of all years are not multiples of 4. For these years only 1 test is done.
- 24% of all years are multiples of 4 but not of 100. For these, only 2 tests are done.
- For the remaining 1% of years, all 3 tests are done.

If we leave out the parentheses, i.e. check for divisibility by 4 and 100 together, the end result is the same but it takes more work to get there:

| year | `divisibleBy 4` | `divisibleBy 100` | `divisibleBy 400` | is leap year |
| ---- | --------------- | ----------------- | ----------------- | ------------ |
| 2020 | `True`          | `False`           | not evaluated     | `True`       |
| 2019 | `False`         | not evaluated     | `False`           | `False`      |
| 2000 | `True`          | `True`            | `True`            | `True`       |
| 1900 | `True`          | `True`            | `False`           | `False`      |

As you can see, this version always does at least 2 tests.


[operators-source-code]:
    https://hackage.haskell.org/package/ghc-prim-0.9.0/docs/src/GHC.Classes.html#line-509
    "source code of the logical operators"
[wikipedia-associativity]:
    https://en.wikipedia.org/wiki/Operator_associativity
    "Wikipedia: Operator associativity"
[wikipedia-non-strict-evaluation]:
    https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_evaluation
    "Wikipedia: Non-strict evaluation"
[wikipedia-precedence]:
    https://en.wikipedia.org/wiki/Order_of_operations
    "Wikipedia: Order of operations"
[wikipedia-truth-table]:
    https://en.wikipedia.org/wiki/Truth_table
    "Wikipedia: Truth table"
