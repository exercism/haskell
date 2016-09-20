## Hints

To complete this exercise, you need to implement the following functions:

- `territories` returns the coordinates (1 based, top left is (1,1))
of the points in each territory along with who "owns" the territory.

- `territoriesFor` returns the territory that contains the coordinate
along with the owner of the territory. If the coordinate does not point
to an empty location, returns *Nothing*.

A territory is owned by one of the players if that player's stones
are the only stones adjacent to the territory.

You will find the type signatures already in place, but it is up to you
to define the functions.
