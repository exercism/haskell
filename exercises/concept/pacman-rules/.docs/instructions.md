# Instructions

In this exercise, you need to translate some rules from the classic game Pac-Man into Haskell functions.

You have four rules to translate, all related to the game states.

> Don't worry about how the arguments are derived, just focus on combining the arguments to return the intended result.

## 1. Define if Pac-Man eats a ghost

Define the `eatsGhost` function that takes two arguments (_whether Pac-Man has a power pellet active_ and _Pac-Man is touching a ghost_) and returns a `Boolean` value when Pac-Man is able to eat the ghost.
The function should return `True` only when Pac-Man has a power pellet active and is touching a ghost.

```Haskell
eatsGhost False True
-- -> False
```

## 2. Define if Pac-Man scores

Define the `scores` function that takes two arguments (_whether Pac-Man is touching a power pellet_ and _Pac-Man is touching a dot_) and returns a `Boolean` value if Pac-Man scored.
The function should return `True` when Pac-Man is touching a power pellet or a dot.

```Haskell
scores True True
-- -> True
```

## 3. Define if Pac-Man loses

Define the `loses` function that takes two arguments (_whether Pac-Man has a power pellet active_ and _Pac-Man is touching a ghost_) and returns a `Boolean` value if Pac-Man loses.
The function should return `True` when Pac-Man is touching a ghost and does not have a power pellet active.

```Haskell
loses False True
-- -> True
```

## 4. Define if Pac-Man wins

Define the `wins` function that takes three arguments (_whether Pac-Man has eaten all of the dots_, _Pac-Man has a power pellet active_, and _Pac-Man is touching a ghost_) and returns a `Boolean` value if Pac-Man wins.
The function should return `True` when Pac-Man has eaten all of the dots _and_ has not lost based on the arguments defined in part 3.

```Haskell
wins False True False
-- -> False
```
