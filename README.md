# lambs

![Scala CI](https://github.com/RaasAhsan/lambs/workflows/Scala%20CI/badge.svg)

A playground for building a statically checked type system based on the lambda calculus. There is merely a definition for an abstract syntax tree and a corresponding type checking function. It would be straightforward to define and implement the operational semantics for the language, but that falls outside the scope of the project. No lexing, parsing, or code generation is performed either. 

I'm also using this playground as an opportunity to try out the Dotty compiler.

TODO:
- user-defined nominal types
- records/structs
- variants
- arrays
- mutability
- function abstraction and application
- parametric polymorphism
- type annotations
- type inference/reconstruction
