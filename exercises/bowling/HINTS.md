## Hints

For a functional language like Haskell it makes sense to calculate the game result in one go from the list of rolls (without an extra `roll` function).
This result can then either be the total game score or an error for a list of irregular rolls.

You will find a data declaration for the possible errors as well as the type signature for the `score` function.
