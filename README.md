# Exercism Haskell Track

[![Build Status](https://travis-ci.org/exercism/haskell.png?branch=master)](https://travis-ci.org/exercism/haskell)

Exercism exercises in Haskell

## Contributing Guide

- [Asking for help](#asking-for-help)
- [How to contribute](#how-to-contribute)
  * [Reporting or fixing bugs](#reporting-or-fixing-bugs)
  * [Reviewing issues and pull requests](#reviewing-issues-and-pull)
  * [Porting exercises](#porting-exercises)
  * [Updating an exercise test suite](#updating-an-exercise-test-suite)
- [Repository structure and conventions](#repository-structure-and-conventions)
  * [Directory structure](#directory-structure)
  * [Exercise structure](#exercise-structure)
- [Writing an issue](#writing-an-issue)
- [Writing a pull request](#writing-a-pull-request)
- [Development Dependencies](#development-dependencies)
- [Stub solution](#stub-solution)
- [Example solution](#example-solution)
- [Test suite](#test-suite)
- [Running tests](#running-tests)
- [Running HLint](#running-hlint)

### Asking for help
When in doubt just ask!
- [gitter support](https://gitter.im/exercism/support): more specific/technical questions.
- [gitter dev](https://gitter.im/exercism/dev): technical questions.

In any case, don't be afraid or shy! We promise to be friendly and as helpful as
possible.

### How to contribute
As a first step we recommend you read the [contributing documentation](https://github.com/exercism/docs/tree/master/contributing-to-language-tracks).

#### Reporting or fixing bugs
Typical examples for a bug: A typo, a missing test case, an unclear or ambiguous
problem description.
- If you are unsure whether you have really found a bug [just ask](#asking-for-help).
- To report a bug you can [write an issue](#writing-an-issue).
- If you already have a fix for it you may [write a pull request](#writing-a-pull-request).

#### Reviewing issues and pull requests
If you have a dedicated opinion you are welcome to [write a comment](https://help.github.com/articles/commenting-on-a-pull-request/) for an [issue](https://github.com/exercism/haskell/issues) or a [pull request](https://github.com/exercism/haskell/pulls).
Please be detailed and include reasons, links or arguments to support your opinion.

#### Porting exercises
The site contains a [list of missing exercises](http://exercism.io/languages/haskell/todo).
You may implement any of these exercises for the Haskell track.
Feel free to refer to the [documentation on porting an exercise](https://github.com/exercism/docs/blob/master/you-can-help/implement-an-exercise-from-specification.md).
Of course you can also add a totally new exercise, but it might be a good idea to first discuss it in one of [our forums](#asking-for-help).

#### Updating an exercise test suite
Updating a test suite of an existing exercise is special because it usually affects all languages.
You can refer to the [documentation on updating a test suite](https://github.com/exercism/x-common/blob/master/CONTRIBUTING.md#updating-an-exercise-test-suite).
Note that the whole test suite must run with the sample solution within a couple of seconds.

### Repository structure and conventions
The [track anatomy documentation](https://github.com/exercism/x-common/blob/master/CONTRIBUTING.md#track-anatomy) is a general description of all the files and directories that are not explicitly described below.

#### Directory structure
```bash
├── .gitignore
├── .travis.yml
├── LICENSE
├── README.md
├── bin
│ └── fetch‐configlet
├── config.json
├── docs
│ ├── ABOUT.md
│ ├── INSTALLATION.md
│ ├── LEARNING.md
│ ├── RESOURCES.md
│ └── TESTS.md
└── exercises
  ├── TRACK_HINTS.md
  └── accumulate
  │ ├── HINTS.md
  │ ├── package.yaml
  │ ├── stack.yaml
  │ ├── examples
  │ │ └── success-standard
  │ │   ├── package.yaml
  │ │   └── src
  │ │     └── Accumuĺate.hs
  │ ├── src
  │ │ └── Accumuĺate.hs
  │ └── test
  │   └── Tests.hs
  └── allergies
  │ ├── ...
  └── ...
```
- `config.json`: Every exercise has to be registered here. It has a unique name and a difficulty. The sequence order is also the default order in which the exercises are fetched.

### Exercise structure
Each exercise has the following structure:
- `stack.yaml` has just one line specifying the current
[Stack snapshot](https://www.stackage.org/snapshots). We use the same
resolver for all the exercises.
- `package.yaml` is a file in the [hpack](https://github.com/sol/hpack#readme)
format that has all dependencies and build instructions for an exercise.
- `HINTS.md` is an optional file containing instructions and/or hints. It is used together with the respective `description.md` for the exercise from [x-common](https://github.com/exercism/x-common) to build the `README.md` file.
- `src/ModuleName.hs` is a [stub solution](#stub-solution).
- `examples/success-<name>/package.yaml` contains library dependencies for the [example solution](#example-solution). `<name>` is a unique name for the example - usually "standard" (as in `success-standard`), but it can be some other name in case of multiple example solutions.
- `examples/success-<name>/src/ModuleName.hs` is the source code of the sample solution.
- `test/Tests.hs` is the [test suite](#test-suite).

### Writing an issue
To report a bug you should [create an issue](https://help.github.com/articles/creating-an-issue/) on the [exercism/haskell repo](https://github.com/exercism/haskell/issues).

### Writing a pull request
To fix a bug you should [create a pull request from a fork](https://help.github.com/articles/creating-a-pull-request-from-a-fork/) on the [exercism/haskell repo](https://github.com/exercism/haskell/pulls).
If you need help with Git, we have some [documentation on Git basics](https://github.com/exercism/docs/blob/master/contributing/git-basics.md).

### Development Dependencies
You should have [Stack](http://docs.haskellstack.org/) installed in your system to make contributing to this repository easier.

### Stub solution
The stub solution should be as general as possible in order to not exclude any possible solutions. It should take Haskell specifics into account (for example use `Maybe` instead of a dummy return value). It should not contain any comments (people might forget to remove them), you can use the hints file instead.

The stub solution must compile by itself (with `stack build`).
Ideally, it would also compile together with the test suite (with `stack test --no-run-tests`).
These two conditions are enforced by Travis.
If the second condition cannot be met for a good reason, place the explanation in `.meta/DONT-TEST-STUB` to circumvent the check.
The first condition is always enforced and cannot be circumvented.

### Example solution
The example solution could be inspiration for other language implementors. It doesn't need to be perfect or very elegant. But it should be efficient enough for the test suite to finish in only a few seconds.

Examples are named `<type>-<name>`.
There are three possible types of examples:

* success: The example is expected to pass the tests.
  There should be at least one of these per exercise.
* fail: The example is expected to build, but fail the tests.
* error: The example is expected to fail to build.

### Test suite
The test suite should be derived from the respective `x-common/exercises/<exercise-name>/canonical-data.json` and comply to some formatting and coding standards (to get an idea you may look at some of the existing tests).

### Running Tests
In order to be accepted by Travis-CI, every exercise must be registered in
`config.json`, it must compile without warnings and the example solution must
pass the tests without failures. Additionally the tests should not run longer than
a few seconds.

First you need to provide an [example solution](#example-solution).

We provide three scripts in the `bin` directory of this repository to run the tests.
These are the same scripts as those used by Travis CI.

* `test-example path/to/example/dir` runs the tests on a single example.
* `test-all-examples path/to/exercise/dir` runs the tests on all examples for an exercise.
* `test-stub path/to/exercise/dir` checks that the stub for the given exercise compiles.

### Running HLint
All code in this repository should be as idiomatic as possible, so we
enforce in Travis-CI that it returns `No hints` when processed by
HLint.

It is highly recommended to run `hlint` on your sources before opening
a [pull request](#writing-a-pull-request), so you can fix your code before submitting it for review.

If you are certain that a suggestion given by `hlint` would make the
code worse, you can [suppress it](https://github.com/ndmitchell/hlint#customizing-the-hints)
with annotations in the source file.


### Haskell icon
The Haskell icon was designed by Darrin Thompson and Jeff Wheeler. It was released under the [HaskellWiki license](https://wiki.haskell.org/HaskellWiki:Copyrights).
