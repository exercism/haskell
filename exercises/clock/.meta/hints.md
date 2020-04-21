Implement a 24 hour clock that handles times.

You should be able to add and subtract hours and minutes to it.

Two clocks that represent the same time should be equal to each other.

## Hints

To complete this exercise you need to define the data type `Clock`,
add an `Eq` instance, and implement the functions:

- addDelta
- fromHourMin
- toString

`addDelta` adds a duration, expressed in hours and minutes, to a given time,
represented by an instance of `Clock`.

`fromHourMin` takes an hour and minute, and returns an instance of `Clock` with 
those hours and minutes.

`toString` takes an instance of `Clock` and returns a string representation 
of the clock, in 0 padded format like "08:03" or "22:35"

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.

If you need help getting started with Types, take a look at:
- [Data Types in 5 Steps][types]

[types]: https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps


