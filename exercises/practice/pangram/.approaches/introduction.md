# Introduction

The key to solving this exercise is checking that all letters of the alphabet are present in the input.
This can be done in a great variety of ways.

Solutions to this problem tend to fall in one of four categories:

- Some solutions iterate over the alphabet first, others over the input.
- Some solutions can deal with infinite input, others cannot.

Why care about whether you can deal with infinite input?
Well, if you cannot, then you are (almost?) certainly doing unnecessary work _somewhere_.
(If your problem is solvable with finite effort, that is.)
If you can deal with infinite input just fine, this is a hint that your strategy is efficient.

Also, reasoning about infinite data can be great fun 😁

All the approaches highlighted above have variants that use the `Set` data structure.
You can find the code in the linked approach documents.


## Approach: express the definition of _pangram_

```haskell
isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) ['a' .. 'z']
```

This solution is simply the definition of _pangram_ put into ~~words~~ code.
A sentence is a pangram when `all` letters of the alphabet are present.

This solution iterates over the alphabet first and tolerates infinite input.
However, it is inefficient for some inputs.
This can be remedied using `Set`, at the cost of losing tolerance for infinite input.

[Read more about this approach][all].


## Approach: check that the alphabet is a substructure of the input

```haskell
isPangram :: String -> Bool
isPangram = (['a' .. 'z'] `isSubsequenceOf`) . sort . map toLower
```

This approach uses `isSubsequenceOf` or `isSubsetOf` to check that the alphabet forms a subsequence/subset of the input.

Due to `sort` or `Set.fromList`, this approach does not tolerate infinite input.
It is efficient though: after sorting/accumulating, both the alphabet and the input are walked only once.

[Read more about this approach][substructure].


## Approach: cross off all characters at once

```haskell
isPangram :: String -> Bool
isPangram text = null (['a' .. 'z'] \\ map toLower text)
```

This solution uses `(\\)` to remove all letters in the input from the alphabet.
Finally, it checks that all letters were removed.

Perhaps surprisingly, this solution does _not_ tolerate infinite input.

[Read more about this approach][cross-off-all].


## Approach: cross off characters as you go and stop when none remain

```haskell
isPangram :: String -> Bool
isPangram = any null . scanl (flip delete) ['a' .. 'z'] . map toLower
```

This approach is the most complicated one on this page.
It is also the most efficient.
In theory at least.

[Read more about this approach][cross-off-one-by-one].


[all]:
    https://exercism.org/tracks/haskell/exercises/pangram/approaches/all
    "Approach: use all to express what a pangram is"
[cross-off-all]:
    https://exercism.org/tracks/haskell/exercises/pangram/approaches/cross-off-all
    "Approach: cross off all characters at once"
[cross-off-one-by-one]:
    https://exercism.org/tracks/haskell/exercises/pangram/approaches/cross-off-one-by-one
    "Approach: cross off characters as you go and stop when none remain"
[substructure]:
    https://exercism.org/tracks/haskell/exercises/pangram/approaches/substructure
    "Approach: check that the alphabet is a substructure of the input"
