Haskell is a purely functional, lazy, statically-typed programming language with type inference.

**Functional** means that functions are first-class data types.

**Purely Functional** means (roughly) that there are no side effects.
Every function will always return the same value for a given argument will do nothing else.

**Lazy** (a.k.a "non-strict") means that the compiler will put off evaluating a thing until absolutely neccessary.
This lets you safely do weird stuff like operating on an infinite list--the language will only create it up to the last value you actually use.

**Statically-typed** means that identifiers have a type set at compile time--like those in Java, C++ or C#--instead of holding data of any type like those in Python, Ruby or JavaScript.

**Type inference** means that the compiler will often figure out the type of an identifier by itself so you don't have to specify it.
Scala and later versions of C# both do this.

In addition, Haskell is standardized and has multiple high-quality implementations, some of which produce standalone native binaries.
There is also a collection of free third-party libraries available and a package manager ("cabal") to automatically fetch them for you.
