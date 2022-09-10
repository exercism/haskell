# Instructions

In this exercise, you are playing a number guessing game with a friend. 
The rules are simple: you secretly choose a number between `1` and `100` and your friend tries to guess what number you've chosen. 
To help your friend, you respond differently depending on how close the guess was to the number you've chosen (`42`). 
These are the rules for the different possible inputs:

- If the guess is `42`: "Correct"
- If the guess is `41` or `43`: "So close"
- If the guess is less than `41`: "Too low"
- If the guess is greater than `43`: "Too high"

You have four tasks to encode the replies to the guesses.

## 1. Reply to a correct guess

Implement the `reply` function to reply to a correct guess:

```haskell
reply 42
-- -> "Correct"
```

## 2. Reply to a close guess

Modify the `reply` function to reply to close guesses:

```haskell
reply 41
-- -> "So close"
```

## 3. Reply to too low guesses

Modify the `reply` function to reply to too low guesses:

```haskell
reply 25
-- -> "Too low"
```

## 4. Reply to too high guesses

Modify the `reply` function to reply to too high guesses:

```haskell
reply 88
-- -> "Too high"
```
