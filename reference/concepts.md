# Haskell reference

## Concepts

The Haskell concept exercises are based on concepts. The list below contains the concepts that have been identified for the Haskell language.

### Language-unique

- ~~Computation expressions~~ do-notation

### Functional

- Expression-oriented (no statements at all)
- Functions
  - Anonymous functions
  - Currying
  - Function composition
  - Higher-order functions
  - Nested functions 🤔
  - Partial application
  - Pure functions
  - Recursion
    - Tail recursion
  - Type declarations & annotations
- Immutability
- Pattern matching
  - Wildcards
  - Guard clauses
- Type inference
  - Automatic generalisation 🤔

### General

- Asynchronous programming
- Collections
  - Collections: combining
  - Collections: filtering (`filter`)
  - Collections: mapping (`Functor`)
  - Collections: ordering (`sort`)
  - Collections: reducing (`Foldable`. `Traversable`)
  - ~~Generics~~ Polymorphism
  - ~~Iterators (yield)~~ Lists
  - Ranges (`Enum` + range syntax)
  - ~~Sequence/for expressions~~ list comprehensions
- Comparison
  - Equality (`Eq`)
  - Ordering  (`Ord`, `Ordering`)
- Concurrency
- Conditionals
  - Boolean logic
  - Conditionals: if
- Conversions
  - Explicit
  - Implicit
- Enumeration
- Exceptions
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
- String formatting
- Values
  - Definitions

### Types

- Booleans
- Characters
- Collections
  - Arrays
  - Lists
  - Maps
  - Queues
  - Sets
  - Stacks
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
