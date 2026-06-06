# Bools

Haskell has a type known as [`Bool`][bools].
It is an enumeration of `True` and `False`.

## Logical operators

Haskell has three logical operators (`not`, `||`, `&&`), which combine Bools and make expressions that produce different values.

### And(`&&`)

The [_and_ operator][and] in Haskell is represented by `&&` and returns `True` if both values are `True`; otherwise, it returns `False`.
When using the _and_ operator, one Bool is placed on the right side of the `&&` and another on the left side.

```haskell
True && True
-- -> True

True && False
-- -> False
```

### Or(`||`)

The [_or_ operator][or] in Haskell is represented by `||` and returns `True` if **at least one** of the values given is `True`. 
If both of the values are `False`, then it returns `False`.
When using the _or_ operator, one Bool should be placed on the right side of the `||` and another on the left.

```haskell
True || True
-- -> True

True || False
-- -> True

False || False
-- -> False
```

### Not(`not`)

The _not_ operator in Haskell is represented by `not` and returns `True` if the given Bool is `False` and returns `False` if `True` is provided.
When using the _not_ operator, one Bool should be placed after the operator (`not`).

```haskell
not True
-- -> False

not False
-- -> True
```

## Using parentheses(`()`)

When working with booleans, you can use parentheses to decide which Bools to evaluate first.
The result can differ depending on how the parentheses are used.
In Haskell, what is in parentheses is evaluated first.

```haskell
True && False && False || True
-- -> True

True && False && (False || True)
-- -> False
```

Since what is in parentheses is evaluated first, the _not_ operator will apply to the expression inside parentheses in the following example.

```haskell
not True && False
-- -> False

not (True && False)
-- -> True
```

~~~~exercism/note
You should only use parentheses when they affect the result; otherwise, they should be omitted.
~~~~

[bools]: https://hackage.haskell.org/package/base/docs/Data-Bool.html
[and]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:-38--38-
[or]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:-124--124-
[not]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:not
