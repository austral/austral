# How the Austral Compiler Works

This document is a walkthrough of every stage in the Austral bootstrapping
compiler's pipeline, from source to target.

# Overview

The overall pipeline looks like this:

![Compiler pipeline](pipeline.png)

# Parsing

Every Austral module is divided into two files: the module interface file and
the module body file.

Each one is parsed into a separate **CST** (concrete syntax tree) by the
functions in the `ParserInterface` module.

# Import Insertion Pass

Each of the module interface and module body are modified to have an implicit
import of all the symbols in the `Austral.Pervasive` module.

# Combining Pass

The combining pass takes the CST for the module interface and for the module
body and combines them into a single representation, where all declarations are
present and have visibility information derived from whether or not they appear
in the module interface.

# Extraction Pass

The extraction pass goes through a combined module and extracts its declarations
into the environment. The bodies of functions and methods are ignored.

# Typing Pass

The typing pass is the largest pass by volume: type checking happens here, and
the untyped AST is converted into a **TAST** (typed abstract syntax tree).

# Lifetime Analysis

This section describes the lifetime analysis pass.

## Linearity Rules

In the lifetime analysis pass, we:

1. Ensure linear values are used once and exactly once.

2. Ensure references to linear values are used within the lifetime of that value.

The lifetime of linear intermediates is unimportant: e.g., `f(g(h(x)))`, if `g`
returns a linear type, then that's a linear value that's immediately created and
consumed. We just ensure that linear intermediates are not discarded, e.g. a
statement like `g(x);` is not valid because the linear value is returned and
discarded, that is, used zero times.

What we care about -- what can be borrowed -- are linear variables.

The lifetime of a linear variable begins where it is defined and ends where it
is consumed:

```
let x: T := f(); -- begins
...
consume(x);      -- ends
```

If variables are consumed zero times, or more than once, that's an error we have
to check.

## Borrowing Rules

There are two kinds of borrows:

1. *Anonymous borrows* where the reference region has no name.

2. *Named borrows* where the reference region has a name and can appear in type
   specifiers.

Anonymous borrows are used where we have to pass a reference to a function, but
the reference (or, more accurately, the reference's region) is not part of the
function's return type. For example, in a function like:

```
generic [T: Type, R: Region]
function Length(list: Reference[List[T, R]]): Size;
```

Essentially we're giving a query function read permission on the data structure,
and we only care about the result, and not some transformation that can be
performed on the reference.

But suppose we have:

```
generic [T: Type, R: Region]
function Nth_Ref(list: Reference[List[T, R]], index: Size): Reference[T, R];
```

That is, given a reference to the list, we get a reference to the _n_-th element.

We can't do this:

```
let nr: Reference[T, ???] := Nth_Ref(&ref, idx);
```

because we don't have the name of the region `&ref`. To do this, we have to use
a named borrow.

A named borrow is a special form of the `let` statement:

```
let <V>: Reference[<T>, <R>] := &<L>;
```

where `V` is the name of the reference variable, `T` is the referenced type, `R`
is the region name, and `L` is the name of a linear variable being
borrowed. This is special because `R` is not known from anywhere else: it is
introduced here.

Here, we can do:

```
let listref: Reference[List[T], R] := &list;
let nthref: Reference[T, R] := Nth_Ref(listref, idx);
```

Because the region name `R` was introduced by the named borrow.

The rules are straightforward:

1. Anonymous borrows of a linear variable must happen within the lifetime of the
   linear variable.

2. Named borrows have a lifetime: from the `let` borrow to the last statement
   where the region introduced appears. This lifetime must be within the
   lifetime the region borrows.

3. Read reference lifetimes can overlap.

4. Write reference lifetimes cannot overlap with each other or with other read
   references to the same variable.

## Data Structures

[TODO]

## Variable Registration Pass

[TODO]

## Variable Appearances Pass

[TODO]

## Region Lifetimes Pass

[TODO]

# Body Extraction Pass

In the body extraction pass, the bodies of functions and instance methods are
extracted from a typed module instance and stored in the environment. We need
the bodies in the environment to instantiate generic functions in the
monomorphization pass.

# Monomorphization Pass

In the monomorphization pass, generic types and functions are instantiated into
concrete types.

# Code Generation

In the code generation pass, a monomorphized module is converted into a C
translation unit.

# Rendering

In the rendering pass, implemented in the `CRender` module, the C code AST is
rendered into C code which is dumped to a file for GCC/Clang to compile.
