# How to implement a Haskell concept exercise

This document describes the steps required to implement a concept exercise for the Haskell track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercise and how they are structured?][docs-concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

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
                ├── package.yaml
                ├── stack.yaml
                ├── src
                │   └── <NAME>.hs
                ├── test
                │   └── Tests.hs
                ├── examples
                │   ├── success-... (default: standard)
                │   │   ├── package.yaml
                │   │   └── src
                │   │       └── <NAME>.hs
                │   └── ...
                ├── .docs
                │   ├── instructions.md
                │   ├── introduction.md
                │   ├── hints.md
                │   └── after.md
                └── .meta
                    ├── config.json
                    └── design.md
```

## Step 1: add track-specific files

These are files specific to the Haskell track:

- `package.yaml`: The Stack project file.
- `stack.yaml`: The Stack configuration file.
- `src/<NAME>.hs`: The stub file being handed to the student.
- `test/Tests.hs`: The test suite being handed to the student.
- `examples/success-...`: A directory containing an example solution.
  - `package.yaml`: That example solution's Stack project file.
  - `src/<NAME>.hs>`: That example solution's source code.

## Step 2: add common files

For the Markdown files in `.docs/` and `.meta/`, see the general advice on [how to implement a concept exercise][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented Haskell exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

[reference]: ../../../reference
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[spring-2013]: https://www.seas.upenn.edu/~cis194/spring13/
[fall-2016]: https://www.seas.upenn.edu/~cis194/fall16/
