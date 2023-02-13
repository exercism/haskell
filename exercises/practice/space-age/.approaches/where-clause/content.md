# Give meaningful names to important values

```haskell
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (periodInEarthYears * secondsPerEarthYear)
  where
    secondsPerEarthYear = 60 * 60 * 24 * 365.25

    periodInEarthYears = case planet of
      Mercury -> 0.2408467
      Venus -> 0.61519726
      Earth -> 1
      Mars -> 1.8808158
      Jupiter -> 11.862615
      Saturn -> 29.447498
      Uranus -> 84.016846
      Neptune -> 164.79132
```

This approach uses a `case` expression to choose the relevant orbital period.
Also a `where` clause is used to give names to important values.
This tends to greatly improve readability.
