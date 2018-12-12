# Austral

[![Build Status](https://travis-ci.com/austral/austral.svg?branch=master)](https://travis-ci.com/austral/austral)

Not done yet.

Austral is a new language in the Lisp family.

Features:

- **Linear types**: linear types allow resources to be handled in a
  provably-safe manner. Memory can be managed safely and without runtime
  overhead, avoiding double `free()` and use-after-`free` errors. Other
  resources like file or database handles can also be handled linearly.

- **Macros**: macros enable compile-time metaprogramming and extending the
  language from within.

- **Typeclasses**: typeclasses, borrowed from Haskell, allow for bounded ad-hoc
  polymorphism.

- **Safe arithmetic**: Austral has well-defined integer modular arithmetic
  semantics, in addition to built-in overflow-checked arithmetic operators and
  saturation arithmetic operators.

- **Algebraic data types**: as in ML or Haskell.

Anti-features:

- No garbage collection.
- No destructors.
- No exceptions.
- No implicit function calls.
- No implicit type conversions.
- No global variables.
- No type inference, type information flows in one direction.

# License

Copyright 2018 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.
