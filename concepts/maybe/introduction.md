# Introduction

Consider the following two scenarios.

-   A web shop maintains a database of customers.
    All customers have an e-mail address registered.
    Also, most have a known postal address.
    However, some customers do not.
    How should this possibly-unknown postal address be represented in the database?

-   Eager to become rich, you write a program for scouring Donald Knuth's books for mistakes, so that you may collect [the bounties][wikipedia-knuth-reward-check].
    After a few days of coding, you end up with a function `findMistake :: Book -> Mistake`.
    However, this cannot possibly be the correct type.
    After all, with Knuth there is a very real risk that there is no mistake at all!
    If it is not certain that a `Mistake` can be found, what should `findMistake`'s return type be instead?

In Haskell we use `Maybe` types to represent possible absence and possible failure.

`Maybe` itself is not a type.
Instead, it is a **type constructor**:  like a function, but for types.
When applied to an type, it will produce a new type.
For example, `Maybe Integer` is a type, and `Maybe Bool` is another type.
In contrast, `Maybe 13` and `Maybe Maybe` are nonsense, as `13` and `Maybe` both aren't types.

All values of a `Maybe` type have one of two shapes.
On the one hand we have `Nothing`, which represents absence or failure.
And on the other hand we have `Just something`, which represents a present value or a successful result.

Some values of type `Maybe Integer` are `Nothing`, `Just (-4)`, and `Just 72`.
There is an infinite number of values of type `Maybe Integer`, namely `Nothing` and one `Just n` for every `n :: Integer`.

Of type `Maybe Bool` there are only three values: `Nothing`, `Just False`, and `Just True`.

Coming back to the example scenarios above:

-   A possibly-unknown address would be represented as a `Maybe Address`.
    An absent address would be denoted by `Nothing`, and a known `address :: Address` would be present in the database as `Just address`.

-   The type of your program should be `findMistake :: Book -> Maybe Mistake`.
    If it finds some `mistake`, it will return `Just mistake`.
    If it does not find any, it will return `Nothing`.

While the `Nothing`s of all the `Maybe` types might look alike, they are not the same because they have different types:

```haskell
noInteger = Nothing :: Maybe Integer
noBool    = Nothing :: Maybe Bool
areTheyEqual = noInteger == noBool
-- error: Couldn't match type ‘Bool’ with ‘Integer’
```


## Producing a `Maybe`

What are these `Nothing` and `Just`, exactly?

We call them **data constructors** (or just _constructors_ for short), because they let us _build values_.
You have seen constructors before: `False` and `True` are the constructors of the `Bool` type.
Also, you can think of &hellip;, `-1`, `0`, `1`, &hellip; as the (infinitely many) constructors of the `Integer` type.

Constructors are just like regular Haskell functions, except

-   They are guaranteed to give different results for different inputs.
    Regular functions, like `square x = x * x`, do not have this property: `square (-6)` and `square 6` return the same value (`36`).
    But constructors do: `Just (-6)` is different from `Just 6`, and the same holds for all other values.
    Conversely, if you ever find that `Just a == Just b`, then you know for certain that `a == b`.
    (This is called [_injectivity_][wikipedia-injectivity].)

-   The values they produce are unique: the _only_ way to produce the value `Just 6` is by applying `Just` to `6`.
    There does not exist another function that can do this without itself using `Just`.
    As a result, if you encounter a `Just 45` you can be certain how it was made.
    We will come back to this when we revisit **pattern matching**.

<!-- TODO: elaborate -->
The type of `Nothing` is `Maybe a`.

<!-- TODO: elaborate -->
The type of `Just` is `a -> Maybe a`


## Using a `Maybe`

The module [`Data.Maybe`][data-maybe] from the standard library contains a number of useful functions for working with `Maybe`s.
Take a look!

<!-- TODO: list a bunch of functions from Data.Maybe -->



[data-maybe]:
    http://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Maybe.html
    "Data.Maybe module documentation"

[wikipedia-factorial]:
    https://en.wikipedia.org/wiki/Factorial
    "Wikipedia: Factorial"

[wikipedia-injectivity]:
    https://en.wikipedia.org/wiki/Injective_function
    "Wikipedia: Injective function"

[wikipedia-knuth-reward-check]:
    https://en.wikipedia.org/wiki/Knuth_reward_check
    "Wikipedia: Knuth reward check"
