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

## Building

Building the `austral` executable requires `make`, [MLton][mlton], and a C
compiler for building the resulting output.

On Debian/Ubuntu, you can install the requisite dependencies with:

```bash
$ sudo apt-get install -y make mlton gcc
```

To build the executable:

```bash
$ make austral
```

To compile in "development mode" (which does not produce an executable binary)
we use [SML/NJ][smlnj], which is faster than MLton and features a REPL. You can
install it in Debian/Ubuntu with:

```bash
$ sudo apt-get install -y smlnj
```

Then, to compile the source code and load up a REPL, run:

```bash
$ make
```

Finally, to run the tests, you need both MLton and SML/NJ. Run this:

```bash
$ ./run-tests.sh
```

## Examples

### Hello, World

This example uses the CFFI extension to call the foreign `puts` function.

```lisp
(austral.ext.cffi:defcfun (puts "puts") ((ptr (paddress u8))) i32)

(defun main () i32
  (puts (static-array-pointer "Hello, world!"))
  0)
```

The first line uses the `defcfun` toplevel form to define an Austral function,
`puts`, that calls the C function `puts`. It defines the argument list, a single
argument of type `(paddress u8)`, and the return type, a 32-bit signed integer.

The second toplevel form defines the program's entrypoint, the traditional
`main` function. The type of string constants is `(static-array u8)`, so we have
to call the `static-array-pointer` function to extract the array's pointer.

### Factorial

### Fibonacci

```lisp
(defun fib ((n i32)) i32
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

## Practical Linear Types

A simple type system like that of C helps us by pointing out errors concerning the shape of data: trying to fit a square peg in a round hole. More advanced type systems help us express the semantic role of data in our types. Linear types go further, and help us prevent a whole category of defects along the lines of use-after-`free` and double-`free` errors.

Consider the following database API, using SML notation:

```sml
val connect : string -> database
val query : database -> string -> result list
val close : database -> unit
```

Briefly, the `connect` function takes a connection URI and returns a `database` instance, the `query` function takes a database instance and a query string and returns the result list of that query, and `close` closes a database connection.

The correct usage pattern is: we open a database connection, make zero or more queries, then close it. But SML's type system does not allow us to enforce this.

For instance, we can use the database after it's been closed:

```sml
close db;
val results = query db "SELECT ...";
```

Or we can close the database twice in a row:

```sml
close db;
close db;
```

These errors are similar to use-after-`free` and double-`free` errors in memory management, respectively. Linear types can help us eliminate this category of errors entirely.

Consider the same API, but in a slightly different type system where prefixing a type name with `!` denotes a linear type. Then:

```sml
val connect : string -> !database
val query : !database -> string -> (result list, database)
val close : !database -> unit
```

Now these errors disappear. Use-after-`close` is impossible, because `close` returns `unit`:

```sml
let val db = connect "my_database"
in
  close db;
  do_something_with db; (* This line is an error because `db` has already been used in the line above *)
end
```

And double-`close` errors are also impossible for the same reason:

```sml
let val db = connect "my_database"
in
  close db;
  close db; (* `db` is used twice *)
end
```

But we can still make multiple queries with the linearly-typed API, because the `query` function returns a tuple of the result set and the new database object. In SML notation:

```sml
let val db = connect "my_database"
in
  let val (results, db') = query "INSERT ..." db
  in
    let val (results, db'') = query "SELECT ..." db'
    in
      close db''
    end
  end
end
```

(You can imagine using something like Haskell's `do` notation to remove the nesting from this example.)

# License

Copyright 2018–2019 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.

[mlton]: http://www.mlton.org/
[smlnj]: https://www.smlnj.org/
