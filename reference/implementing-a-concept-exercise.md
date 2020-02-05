# How to implement a Haskell concept exercise

This document describes the steps required to implement a concept exercise for the Haskell track. As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `monad-transformers`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `MonadTransformers`).

Haskell covers a vast amount of concepts. Choosing a particular subset of concepts and an ideal ordering of these concepts is a contentious task, so the following approach is made: As a basis for both, the online resources for the CIS 194 course at Penn University is chosen.

Contributors (you) may freely choose a concept related to Haskell and implement an exercise that covers this. Before implementing such an exercise, make sure you have a good understanding of what the exercise should be teaching (and what not). Check if this concept is already partially or fully covered by another exercise, and arrange the concept in a hierarchical fashion that corresponds to the curriculum of CIS 194.

Because CIS 194 online course material is available in the [Spring 2013][spring-2013] and the [Fall 2016][fall-2016] version, and because these have different orderings, this does leave room for interpretation.

Because CIS 194 may not cover all subject that contributors wish to cover in this material, extending the concepts beyond the CIS 194 course should ideally be accompanied by some level of reasoning about the curriculum.

The directory structure of a single concept exercise looks like this:

```
languages
└── Haskell
    └── exercises
        └── concept
            └── <SLUG>
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md (optional)
                ├── .meta
                |   |── config.json
                |   |── design.md
                |   └── <SLUG>.hs (example solution)
                ├── src
                |   └── <NAME>.hs
                ├── test
                |   └── Tests.hs
                ├── package.yaml
                └── stack.yaml
```

## Step 1: add track-specific files

These are files specific to the Haskell track:

- `src/<NAME>.hs`: the stub implementation file, which is the starting point for students to work on the exercise.
- `test/Tests.hs`: the test suite.
- `.meta/Example.hs`: an example implementation that passes all the tests.
- `stack.yaml`: the stack project file.
- `package.yaml`: the cabal package file.

## Step 2: add common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented Haskell exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

[reference]: ../../../reference
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[spring-2013]: https://www.seas.upenn.edu/~cis194/spring13/
[fall-2016]: https://www.seas.upenn.edu/~cis194/fall16/
