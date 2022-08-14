# Exercism Haskell Track

[![Tests](https://github.com/exercism/haskell/workflows/Tests/badge.svg?branch=main)](https://github.com/exercism/haskell/actions?query=workflow%3ATests+branch%3Amain)

Exercism exercises in Haskell

- [How to contribute](#how-to-contribute)
  * [Git and GitHub](#git-and-github)
  * [Report or fix a bug](#report-or-fix-a-bug)
  * [Review issues and pull requests](#review-issues-and-pull)
  * [Port or create an exercise](#port-or-create-an-exercise)
  * [Port or create a concept](#port-or-create-a-concept)
  * [Port or create a concept exercise](#port-or-create-a-concept-exercise)
  * [Update an exercise test suite](#update-an-exercise-test-suite)
- [Repository structure and conventions](#repository-structure-and-conventions)
  * [Directory structure](#directory-structure)
  * [Exercise structure](#exercise-structure)
  * [Exercise versioning](#exercise-versioning)
- [Development Dependencies](#development-dependencies)
- [Stub solution](#stub-solution)
- [Example solution](#example-solution)
- [Test suite](#test-suite)
- [Running tests](#running-tests)
- [Running HLint](#running-hlint)
- [Automated Test Runner](#automated-test-runner)

## How to contribute

### Git and GitHub
If you would like to contribute but lack experience with git and/or GitHub, try these resources:

- [GitHub Guides: Understanding the GitHub flow](https://guides.github.com/introduction/flow/)
- [GitHub Help: Creating a pull request from a fork](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
- [Exercism's Git Basics](https://github.com/exercism/docs/blob/master/contributing/git-basics.md)

### Report or fix a bug
Typical examples for a bug: A typo, a missing test case, an unclear or ambiguous problem description.

- If you are unsure whether you have really found a bug, [just ask](https://github.com/exercism/haskell/issues/new).
- To report a bug you can [create an issue](https://github.com/exercism/haskell/issues/new).
- If you already have a fix for it, you may submit a pull request.

### Review issues and pull requests
If you have a dedicated opinion you are welcome to [write a comment](https://help.github.com/articles/commenting-on-a-pull-request/) for an [issue](https://github.com/exercism/haskell/issues) or a [pull request](https://github.com/exercism/haskell/pulls).

Please be detailed and include reasons, links or arguments to support your opinion.

### Port or create an exercise

Exercism contains two types of exercises: concept exercises, and practice exercises.

Haskell has some concept exercises.
You can read about [concept exercises](reference/implementing-a-concept-exercise.md) and take part in creating Haskell's learning track.

You can get a full list of [common Exercism practice exercises](https://github.com/exercism/problem-specifications/tree/main/exercises) and cross-reference it with [Haskell practice exercises](https://github.com/exercism/haskell/tree/main/exercises/practice) and implement any of the missing ones for the Haskell track.

### Port or create a concept
Concepts are short tutorials explaining a single feature of the language.
The Haskell track has a few concepts currently developed and a list of additional concepts yet to be created.
You can contribute by porting (from the F# or Elm tracks for example) or developing any of the topics listed in [reference/concepts.md](reference/concepts.md)

### Port or create a concept exercise
Each concept is accompanied by a concept exercise to test the student understood the basic use of the concept and unlock the next concept(s).
To develop concept exercises see [reference/implementing-a-concept-exercise.md](reference/implementing-a-concept-exercise.md)

### Update an exercise test suite
Most unit tests are shared between language tracks. You may update a test suite with new unit tests.

- Read about [pushing new unit tests to Exercism](https://github.com/exercism/docs/blob/master/contributing-to-language-tracks/README.md#test-suites).
- See a list of [exercises from which new unit tests can be pulled to this track](https://tracks.exercism.io/haskell/master/versions).
- Read about [test suite principles](https://github.com/exercism/docs/blob/master/language-tracks/exercises/anatomy/test-suites.md).

Note that the whole test suite must run with the sample solution within a couple of seconds.

## Repository structure and conventions
The [track anatomy documentation](https://github.com/exercism/docs/blob/master/language-tracks/README.md) is a general description of all the files and directories that are not explicitly described below.

### Directory structure
```bash
├── .gitignore
├── .github
│ └── workflows
│     └── tests.yml
├── LICENSE
├── README.md
├── bin
│ └── fetch‐configlet
├── config.json
├── docs
│ ├── ABOUT.md
  ├── EXERCISE_README_INSERT.md
│ ├── INSTALLATION.md
│ ├── LEARNING.md
│ ├── RESOURCES.md
│ └── TESTS.md
└── exercises
  └── accumulate
  │ ├── package.yaml
  │ ├── stack.yaml
  │ ├── src
  │ │ └── Accumuĺate.hs
  │ ├── test
  │ │ └── Tests.hs
  │ └── .meta
  │   ├── examples
  │   │ └── success-standard
  │   │   ├── package.yaml
  │   │   └── src
  │   │     └── Accumulate.hs
  │   └── hints.md
  └── allergies
  │ ├── ...
  └── ...
```
- `config.json`: Every exercise has to be registered here. It has a unique name and a difficulty. The sequence order is also the default order in which the exercises are fetched.

## Exercise structure
Each exercise has the following structure:
- `stack.yaml` has just one line specifying the current
[Stack snapshot](https://www.stackage.org/snapshots). We use the same
resolver for all the exercises.
- `package.yaml` is a file in the [hpack](https://github.com/sol/hpack#readme)
format that has all dependencies and build instructions for an exercise.
  One of the properties tracked in `package.yaml` is the [version](#exercise-versioning) of the exercise.
- `src/ModuleName.hs` is a [stub solution](#stub-solution).
- `.meta/examples/success-<name>/package.yaml` contains library dependencies for the [example solution](#example-solution). `<name>` is a unique name for the example - usually "standard" (as in `success-standard`), but it can be some other name in case of multiple example solutions.
- `.meta/examples/success-<name>/src/ModuleName.hs` is the source code of the sample solution.
- `test/Tests.hs` is the [test suite](#test-suite).
- `.meta/hints.md` is an optional file containing instructions and/or hints. It is used together with the respective `description.md` for the exercise from [problem-specifications](https://github.com/exercism/problem-specifications) to build the `README.md` file.

## Exercise versioning

Each exercise contains a four-part version in its `package.yaml` file, MAJOR.MINOR.PATCH.SERIAL.

There are two possibilities for the meaning of the MAJOR.MINOR.PATCH components:

* Exercises based on a `canonical-data.json` in [problem-specifications](https://github.com/exercism/problem-specifications) should use its version plus a serial number.
* Exercises that are not based on `canonical-data.json` should use version 0.1.0 plus a serial number.

The serial number starts at 1 and always increases when the tests are changed, regardless of the changes in other version numbers.

When changing a test suite, the version number should be updated appropriately so that:

* It is possible for maintainers of this track to tell whether test suites are up to date with https://github.com/exercism/problem-specifications.
* It is easier for students to determine at-a-glance whether they have the same tests, by comparing version numbers.

This versioning policy was proposed and accepted in https://github.com/exercism/haskell/issues/522.

## Development Dependencies
You should have [Stack](http://docs.haskellstack.org/) installed in your system to make contributing to this repository easier.

## Stub solution
The stub solution should be as general as possible in order to not exclude any possible solutions. It should take Haskell specifics into account (for example use `Maybe` instead of a dummy return value). It should not contain any comments (people might forget to remove them), you can use the hints file instead.

The stub solution must compile by itself (with `stack build`).
Ideally, it would also compile together with the test suite (with `stack test --no-run-tests`).
These two conditions are enforced by GitHub Actions CI.
If the second condition cannot be met for a good reason, place the explanation in `.meta/DONT-TEST-STUB` to circumvent the check.
The first condition is always enforced and cannot be circumvented.

## Example solution
The example solution could be inspiration for other language implementors. It doesn't need to be perfect or very elegant. But it should be efficient enough for the test suite to finish in only a few seconds.

Examples are named `<type>-<name>`.
There are three possible types of examples:

* success: The example is expected to pass the tests.
  * There _must_ be at least `success` example per exercise, in order to confirm that it is possible to solve the tests.
  * There _may_ be more than one `success` example for a given exercise, but these are intended for use when we want to confirm that multiple type signatures for a given solution will compile and pass the tests.
  * We do not intend to use multiple `success` examples just to showcase a wide variety of possible solutions, since that is not in the goals of this repository.
* fail: The example is expected to build, but fail the tests.
  * These are intended for use when we want to make sure that the track tests have coverage: Whether the tests find certain classes of incorrect or inefficient solutions.
  * It's suggested that these only be used for tests that are specific to the track. This is under the assumption that tests sourced from problem-specifications have already been judged to have appropriate coverage by the reviewers of the problem-specifications repository.
* error: The example is expected to fail to build.
  * There is only one intended use of this so far, and that is a single check that a solution without a type signature will fail to build (because CI builds with `--pedantic`).
  * We do not intend for any additional uses of this type of example.

These example types were proposed and accepted in https://github.com/exercism/haskell/issues/397.

## Test suite
The test suite should be derived from the respective `problem-specifications/exercises/<exercise-name>/canonical-data.json` and comply to some formatting and coding standards (to get an idea you may look at some of the existing tests).

## Running Tests
In order to be accepted by GitHub Actions, every exercise must be registered in
`config.json`, it must compile without warnings and the example solution must
pass the tests without failures. Additionally the tests should not run longer than
a few seconds.

First you need to provide an [example solution](#example-solution).

We provide three scripts in the `bin` directory of this repository to run the tests.
These are the same scripts as those used by GitHub Actions.

* `test-example path/to/example/dir` runs the tests on a single example.
* `test-all-examples path/to/exercise/dir` runs the tests on all examples for an exercise.
* `test-stub path/to/exercise/dir` checks that the stub for the given exercise compiles.

## Running HLint
All code in this repository should be as idiomatic as possible, so we
enforce in GitHub Actions that it returns `No hints` when processed by
HLint.

It is highly recommended to run `hlint` on your sources before opening a
pull request, so you can fix your code before submitting it for review.

If you are certain that a suggestion given by `hlint` would make the
code worse, you can [suppress it](https://github.com/ndmitchell/hlint#customizing-the-hints)
with annotations in the source file.

## Automated Test Runner
We have a [test runner](https://github.com/exercism/haskell-test-runner) to automatically run tests on Haskell solutions submitted to [exercism](exercism.org).
