# Haskell reference

## Concepts

The Haskell concept exercises are based on concepts. The list below contains the concepts that have been identified for the Haskell language.

### Language-unique

- ~~Active patterns~~ Pattern synonyms? That seems a stretch.
- ~~Computation expressions~~ do-notation
- Dependency order

### Functional

- Expression-oriented (no statements at all)
- Functions
  - Anonymous functions
  - Currying
  - Function composition
  - Higher-order functions
  - ~~Implicit returns~~ (no returns at all)
  - Nested functions 🤔
  - Partial application
  - Pure functions
  - Recursion
    - Tail recursion
  - Type declarations & annotations
- Immutability
- Pattern matching
  - ~~Discards~~ wildcards
  - Guard clauses
- ~~Pipelines~~ (just composition)
- Type inference
  - Automatic generalisation 🤔

### General

- Asynchronous programming
- Collections
  - Collections: combining 🤔 `<>` ?
  - Collections: filtering (`filter`)
  - Collections: mapping (`Functor`)
  - Collections: ordering (`sort`)
  - Collections: reducing (`Foldable` ?)
  - ~~Generics~~ Polymorphism
  - ~~Iterators (yield)~~ Lists
  - Ranges (`Enum` + range syntax)
    - ~~Slicing~~ 🤔
  - ~~Sequence/for expressions~~ list comprehensions
- Comparison
  - Equality (`Eq`)
  - Ordering  (`Ord`, `Ordering`)
- Concurrency 🤔 (no expertise here)
  - Concurrent collections
  - Locks
  - Messaging and agents
- Conditionals
  - Boolean logic
  - Conditionals: if
- Conversions
  - Explicit
  - Implicit
- Enumeration
- Exceptions 🤔
- Numbers
  - Arithmetic overflow
  - Bitwise manipulation
  - Math operators
- Resources
  - Resource allocation
- Scoping
  - Imports
  - Modules
  - Namespaces
  - Shadowing
  - Visibility (export or not)
- Whitespace significant
- String formatting 🤔
- Values
  - Definitions
- Concurrency

### Types

- Booleans
- Characters
- Collections
  - Arrays 🤔
  - Lists
  - Maps
  - Queues
  - Ranges 🤔 (just lists)
  - Sets
  - Stacks 🤔 (just lists)
- ~~Discriminated unions~~ Algebraic data types
- Numbers (`Num` & Co.)
  - Floating point numbers
  - Signed integers (`Int`, `Integer`)
  - Unsigned integers (`Natural`)
- ~~Options~~ `Maybe`
- Records
- ~~Results~~ `Either`
- Strings
- Tuples
- Unit (`()`)
