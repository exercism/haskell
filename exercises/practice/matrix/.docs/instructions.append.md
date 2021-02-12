## Hints

To complete this exercise, you need to create the data type `Matrix`,
with `Eq` and `Show` instances, and implement the following functions:

- `cols`
- `column`
- `flatten`
- `fromList`
- `fromString`
- `reshape`
- `row`
- `rows`
- `shape`
- `transpose`

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.

No validation of input is required. Let it fail if the matrix is not
rectangular, invalid chars are encountered, etc.

shape is (rows, cols)
