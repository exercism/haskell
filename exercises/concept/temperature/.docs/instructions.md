# Instructions

You are part of a firefighter organization made of international volunteers.
Due to the international nature of the team, valuable time has been wasted over the last few calls translating between Fahrenheit and Celcius while estimating the temperature inside the burning buildings.
To save time, and people, you decide to create a program to automatically convert between Fahrenheit and Celsius for your international team.

## 1. Convert Farenheit to Celsius

Implement the `tempToC` function to convert an `Integer` temperature in Fahrenheit to its equivalent `Float` in Celsius rounded to the nearest hundredth.
Temperature in degrees Celsius is defined as: T(°C) = (T(°F) - 32) / 1.8.

```haskell
> tempToC 32
0.00
```

## 2. Convert Celsius to Farenheit

Implement the `tempToF` function to convert a `Float` temperature in Celsius to its equivalent in Fahrenheit.
For safety reasons, **round up** the result to the **next-highest** `Integer`.
Temperature in degrees Farenheit is defined as: T(°F) = T(°C) × 1.8 + 32.

```Haskell
> tempToF 4
40
```
