# Austral

![Build status badge](https://github.com/austral/austral/actions/workflows/build-and-test.yml/badge.svg)

**Not done yet.**

Austral is a new language.

Features:

- **Linear types**: linear types allow resources to be handled in a
  provably-safe manner. Memory can be managed safely and without runtime
  overhead, avoiding double `free()`, use-after-`free` errors, and double fetch
  errors. Other resources like file or database handles can also be handled
  safely.

- **Capabilities**: linear [capabilities][cap] enable fine-grained permissioned
  access to low-level facilities. Third-party dependencies can be constrained in
  what types of resources they can access. This makes the language less
  vulnerable to [supply chain attacks][sca].

- **Typeclasses**: typeclasses, borrowed from Haskell, allow for bounded ad-hoc
  polymorphism.

- **Safe Arithmetic**: Austral has well-defined semantics for all arithmetic
  operations on numeric types. There are distinct operations for
  trap-on-overflow arithmetic and modular arithmetic, as in Ada.

- **Algebraic Data Types**: algebraic data types, as in ML or Haskell, with
  exhaustiveness checking.

Anti-features:

- No garbage collection.
- No destructors.
- No exceptions (and no surprise control flow).
- No implicit function calls.
- No implicit type conversions.
- No global state.
- No subtyping.
- No macros.
- No reflection.
- No Java-style @Annotations.
- No type inference, type information flows in one direction.
- No uninitialized variables.
- No pre/post increment/decrement (`x++` in C).
- No first-class async.
- No function overloading (except through typeclasses, where it is bounded).
- No arithmetic precedence.

## Example

Calculate and print the 10th Fibonacci number:

```
module body Fib is
    function fib(n: Nat64): Nat64 is
        if n < 2 then
            return n;
        else
            return fib(n - 1) + fib(n - 2);
        end if;
    end;

    function main(): ExitCode is
        print("fib(10) = ");
        printLn(fib(10));
        return ExitSuccess();
    end;
end module body.
```

Build and run:

```bash
$ austral compile --public-module=fib.aum --entrypoint=Fib:main --output=fib.c
$ gcc fib.c -o fib
$ ./fib
fib(10) = 55
```

## Building

Building the `austral` compiler requires `make` and the `dune` build system for
OCaml, and a C compiler for building the resulting output. You should install OCaml
4.12.0 or above.

First, install [opam][opam]. On Debian/Ubuntu you can just do `sudo apt-get install opam`.

Then:

```bash
$ git clone git@github.com:austral/austral.git
$ cd austral
$ ./install-ocaml-deps.sh
$ make
```

To run the tests:

```bash
$ ./run-tests.sh
```

## Usage

Suppose you have a program with modules `A`, `B`, and `C`, in the following
files:

```
src/A.aui
src/A.aum

src/B.aui
src/B.aum

src/C.aui
src/C.aum
```

To compile this, run:

```
$ austral compile \
    --module=src/A.aui,src/A.aum \
    --module=src/B.aui,src/B.aum \
    --module=src/C.aui,src/C.aum \
    --entrypoint=C:main \
    --output=program.c
```

This is the most general invocation. Where module interface and module body
files share the same path and name and differ only by their extension, you can
use:

```
$ austral compile \
    --module=src/A \
    --module=src/B \
    --module=src/C \
    --entrypoint=C:main \
    --output=program.c
```

The order in which `--module` options appear is the order in which they are
compiled, so it matters.

The `--entrypoint` option must be the name of a module, followed by a colon,
followed by the name of a public function with either of the following
signatures:

1. `function main(): ExitCode;`
2. `function main(root: RootCapability): ExitCode;`

The `ExitCode` type has two constructors: `ExitSuccess()` and `ExitFailure()`.

Finally, the `--output` option is just the path to dump the compiled C to.

There's also a command to typecheck a program without outputting anything. Only
`--module` flags are accepted:

```bash
$ austral typecheck \
    --module=src/A \
    --module=src/B \
    --module=src/C
```

The generated code should be compiled with:

```
gcc -fwrapv generated.c -lm
```

## Status

1. The bootstrapping compiler, written in OCaml, is implemented. The main
   limitation is it does not support separate compilation. In practice this is
   not a problem: there's not enough Austral code for this to matter.

2. A standard library with a few basic data structures and capability-based
   filesystem access is being designed.

# License

Copyright 2018–2022 [Fernando Borretti][fernando].

Licensed under the GPLv3 license. See the COPYING file for details.

[opam]: https://opam.ocaml.org/doc/Install.html
[cap]: https://en.wikipedia.org/wiki/Capability-based_security
[sca]: https://en.wikipedia.org/wiki/Supply_chain_attack
[fernando]: https://borretti.me/
