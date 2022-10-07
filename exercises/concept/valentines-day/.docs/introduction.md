# Introduction

An Algebraic Data Type (ADT) represents a fixed number of named cases.
Each value of an ADT corresponds to exactly one of the named cases.

An ADT is defined using the `data` keyword, with cases separated by pipe (`|`) characters.
If none of the cases have data associated with them the ADT is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

```haskell
data Season
  = Spring
  | Summer
  | Autumn
  | Winter
```

Each case of an ADT can optionally have data associated with it, and different cases can have different types of data.
When the case has data associated, a constructor is required.

```haskell
data Number
  = NInt Int      --'NInt' is the constructor for an Int Number.
  | NFloat Float  --'NFloat' is the constructor for an Float Number.
  | Invalid       --'Invalid' does not have data associated to it.
```

Creating a value for a specific case can be done by referring to its name (e.g, `NInt 22`).
As case names are just constructor functions, associated data can be passed as a regular function argument.

ADTs have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

While one can use `if/else` expressions to work with ADTs, the recommended way to work with them is through pattern matching using _case_ statement:

```haskell
add1 :: Number -> String
add1 number =
    case number of
      NInt    i -> show (i + 1)
      NFloat  f -> show (f + 1.0)
      Invalid   -> error "Invalid input"
```
