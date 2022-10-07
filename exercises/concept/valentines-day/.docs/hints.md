# Hints

## 1. Define the approval

- [Define the algebraic data type][ADT] `Approval` with constructors for the required options.

## 2. Define the cuisine

- [Define the algebraic data type][ADT] `Cuisine` with constructors for the required options.

## 3. Define the movie genres

- [Define the algebraic data type][ADT] `Genre` with constructors for the required options.

## 4. Define the activity

- [Define an algebraic data type with associated data][ADT-with-data] to encapsulate the different activities.

## 5. Rate the activity

- The best way to execute logic based on the activity's value is to use [case expressions][case-expression].
- Pattern matching an algebraic data type case provides access to its associated data.
- To add an additional condition to a pattern, you can use a [guard][guards] inside a case.
- If you want to catch all other possible values in one case, you can use the wildcard pattern `_`.

[ADT]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types#enumeration-types
[ADT-with-data]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types#beyond-enumerations
[case-expression]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types#case-expessions
[guards]: https://learnyouahaskell.github.io/syntax-in-functions.html#guards-guards
