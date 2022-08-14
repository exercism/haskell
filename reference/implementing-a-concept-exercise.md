# How to implement a Haskell concept exercise

This document describes the steps to implement a concept exercise for the Haskell track.

- [What are concept exercise and how they are structured?][docs-concept-exercises]
- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise] (video!)

As this document is generic, the following placeholders are used:

- `<EXERCISE-SLUG>`: the slug of the exercise in kebab-case (e.g. `calculator-conundrum`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `CalculatorConundrum`).
- `<CONCEPT-SLUG>`: the slug of one of the exercise's concepts in kebab-case (e.g. `monad-transformers`).

Haskell covers a vast amount of concepts. Choosing a particular subset of concepts and an ideal ordering of these concepts is a contentious task.

Contributors (you) may freely choose a concept related to Haskell and implement an exercise that covers this. Before implementing such an exercise, make sure you have a good understanding of what the exercise should be teaching (and what not). Check if this concept is already partially or fully covered by another exercise, and arrange the concept in a hierarchical fashion.

The directory structure of a single concept exercise looks like this:

```
languages
└── haskell
    ├── concepts
    |   └── <CONCEPT-SLUG>
    |       ├── about.md
    |       ├── introduction.md
    |       └── links.json
    └── exercises
        └── concept
            └── <EXERCISE-SLUG>
                ├── package.yaml
                ├── stack.yaml
                ├── src
                │   └── <NAME>.hs
                ├── test
                │   └── Tests.hs
                ├── .docs
                │   ├── instructions.md
                │   ├── introduction.md
                │   └── hints.md
                └── .meta
                    ├── config.json
                    ├── design.md
                    └── exemplar
                        ├── package.yaml
                        └── src
                            └── <NAME>.hs
```

## Step 1: Add code files

These are files specific to the Haskell track:

- `package.yaml`: The Stack project file.
- `stack.yaml`: The Stack configuration file.
- `src/<NAME>.hs`: The stub file being handed to the student.
- `test/Tests.hs`: The test suite being handed to the student.
- `exemplars/success-...`: A directory containing an exemplar solution.
  - `package.yaml`: That example solution's Stack project file.
  - `src/<NAME>.hs>`: That example solution's source code.

## Step 2: Add documentation files

See [documentation files][docs-concept-exercises-documentation-files].

[reference]: https://github.com/exercism/v3/blob/main/reference
[docs-concept-exercises]: https://github.com/exercism/v3/blob/main/docs/concept-exercises.md
[docs-concept-exercises-documentation-files]: https://github.com/exercism/v3/blob/main/docs/concept-exercises.md#documentation-files
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
