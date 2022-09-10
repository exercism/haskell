# Hints

The type
[`Ordering`](https://hackage.haskell.org/package/base/docs/Data-Ord.html#t:Ordering)
has three constructors, `LT` ("less than"), `EQ` ("equals") and `GT` ("greater
than"). These can represent sublist ordering with `Just LT` meaning "sublist",
and so on, and `Nothing` meaning not a sublist, superlist or equal to.
