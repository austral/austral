# Contributing

This document describes how and where to contribute to Austral.

Communication channels:

- [GithHub Issues][issues]

[issues]: https://github.com/austral/austral/issues

## How to Contribute

For bug reports, questions, and feature requests, please open an issue
[here][issues].

## Development Environment

Short version:

```bash
$ sudo apt-get install opam
$ git clone git@github.com:austral/austral.git
$ cd austral
$ ./install-ocaml-deps.sh
$ make
$ ./run-tests.sh
```

## Contribution Areas

This section documents the areas where contributions are currently needed.

### Compiler Diagnostics

- **Where:**
  - https://github.com/austral/austral/tree/master/lib
  - https://github.com/austral/austral/blob/master/lib/Error.mli
  - https://github.com/austral/austral/blob/master/lib/ErrorText.mli

The primary purpose of a compiler is to provide clear and useful diagnostics and
error messages.

This area is one where improvements can always be made, so there's infinite room
for contributions. Also, there is plenty of low-hanging fruit: many of the fixes
in this area are simple and can have a big impact on the user experience.

The infrastructure for writing structured, informative error messages is already
in place, so this is simply a matter of putting in the effort to identify and
fix problems with the current error messages.

Contributions from the community are most welcome. If you don't have the time or
knowledge to open a PR to fix an error message, it is good enough to just open
an issue to raise than an error message you encountered was confusing or had
insufficient context.

Specific contribution areas:

- **Improvement:** improving existing error messages by clarifying the text and
  adding more contextual information.

- **Editing:** clarifying the prose in error messages to make them clearer to
  understand.

Open issues:

- https://github.com/austral/austral/issues/302

### Tutorials

- **Where:** https://github.com/austral/austral.github.io/tree/master/tutorial

The [tutorials][tut] are likely to be people's first real encounter with the
language. Currently they're fairly spartan, and could use more examples and a
bit more prose.

It is also useful to simply read the tutorials, and point out any areas where
things could be explained more clearly.

Specific contribution areas:

- **Examples:** adding more examples to illustrate different concepts and
  features of the language.

- **Review:** reviewing the current tutorials for clarity and simplicity.

- **Omissions:** find places where language features or rules are not explained
  (the source for these is the [spec][spec]).

[tut]: https://austral-lang.org/tutorial/
[spec]: https://austral-lang.org/spec/

### Example Programs

- **Where:**
  - https://github.com/austral/austral/tree/master/examples
  - https://github.com/austral/austral.github.io/tree/master/_includes/examples
  - https://github.com/austral/austral.github.io/tree/master/examples

There is a small collection of example programs that demonstrate various
language features and how to solve common tasks (e.g. Fibonacci, hello
world). We are always looking for new programs to showcase Austral.

If you've written an Austral program you think is useful, interesting, or
representative; and want to share it, feel free to open a PR or an
[issue][issues].

If you have an existing small program you think would be a useful demo, you can
try porting it to Austral and opening an PR.

We appreciate any and all contributions. The current collection of examples is
small, but we hope it will grow and help new users understand the language.

### Standard Library

- **Where:**
  - https://github.com/austral/austral/tree/master/standard

The standard library largely does not exist. So contributions here involve:

- Thinking and debating what the APIs should be.
- Thinking about how well-known data structures and interfaces from other
  programming languages have to be adjusted to leverage linear types and
  capability-based security (e.g.: what does a capability-secure filesystem API
  look like?).
- Implementing the standard library interfaces.
- Writing unit tests for the existing standard library code.

If you have strong opinions about standard library APIs, or if you know any
mistakes that other programming languages have made that you don't want to see
repeated, we want to hear from you. Or if you just like implementing data
structures, that's useful too.

Since both linear types and capability-based security are fairly unique
features, there's not much prior art here to imitate, so we welcome
contributions from everyone as well as new ideas and approaches.


### Compiler Documentation

- **Where:**
  - High-level documentation: https://github.com/austral/austral/tree/master/docs
  - Source code documentation: https://github.com/austral/austral/tree/master/lib

Compiler documentation is important. Devs have to be able to understand how the
compiler works in order to contribute to it.

The current documentation is somewhat uneven, with some modules being very
thoroughly documented and others having little or no documentation. We are
seeking contributions from the community to help improve the documentation by
adding more information and clarifying existing documentation.

Most of the documentation is in the OCaml source code, using odoc. There are
some Markdown files in the `docs` folder containing higher-level compiler
documentation.

Specific contribution areas:

- **Writing:** adding documentation for modules that currently have little or no
  documentation.

- **Reviewing:** and clarifying existing documentation to make it easier to
  understand.

## Compiler Tests

- **Where:**
  - https://github.com/austral/austral/tree/master/test-programs

The compiler's tests are key to ensuring the compiler faithfully implements the
[spec][spec].

There's a small test runner written in Python, the tests are end-to-end and test
the compiler externally, that is, by running the executable binary. This allows
us to change the internals freely, since the external interface remains the
same.

Each test consists of some Austral code, along with the expected output of
running that code, if it's a successful test (i.e. meant to compile and run
successfully), or the expected compiler error message, if it's a test that's
meant to fail to compile.

# Testing

Every push runs a [GitHub Actions workflow][ci]. This builds the compiler and
runs the OCaml tests, the end-to-end tests, the examples, and the standard
library tests.

To run the tests manually, just run:

```bash
$ ./run-tests.sh
```

[ci]: https://github.com/austral/austral/blob/master/.github/workflows/build-and-test.yml

# Reviews

Just assign [`eudoxia0`][eudoxia].

[eudoxia]: https://github.com/eudoxia0/
