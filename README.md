# lambs

![Scala CI](https://github.com/RaasAhsan/lambs/workflows/Scala%20CI/badge.svg)

A playground for building a statically checked type system based on the simply typed lambda calculus. In the source code, there is merely a definition for an abstract syntax tree and a corresponding type checking function. It would be straightforward to define and implement the operational semantics for the language, but that falls outside the scope of the project. No lexing, parsing, or code generation is performed, however a parse syntax tree is defined to facilitate syntactic sugar. 

I'm also using this playground as an opportunity to try out the Dotty compiler.

## Features
1. System F first-class polymorphism
2. Function abstraction and application
3. Boolean and integer logic
4. N-ary tuples
5. Structural labeled records
6. Statements
7. Let bindings

## TODO
- user-defined nominal types
- records/structs
- variants
- arrays
- mutability
- type annotations
- type inference/reconstruction
