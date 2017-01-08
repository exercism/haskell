## Hints

To complete this exercise you need to define the data type `Clock`,
with `Eq`, `Show` and `Num` instances, and implement the functions:

- clockHour
- clockMin
- fromHourMin
- toString

The function `fromInteger`, from `Num`, must convert minutes
to 24 hour clock time. It is not necessary to have a sensible
implementation of `abs` or `signum`.

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.
