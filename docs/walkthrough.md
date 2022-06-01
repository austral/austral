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

## Positions

A lifetime is a span of code. A span has a beginning and an end. Therefore, we
need a way to represent positions in code, such that we can compare whether two
intervals overlap, or one contains the other, etc. This turns out to be
surprisingly simple to do: a depth first pass is all it takes.

Consider this code:

```
function Foo(x: T): Y is
    let y: U := f(x);
    while cond() do
        foo();
    end while;
    if g(x) then
        foo();
    else if h(x) then
        bar();
    else
        baz();
    end if;
    y := x;
    return x;
end;
```

After annotating positions:

```
function Foo(x: T): Y is  -- 0
    let y: U := f(x);     -- 1
    while cond() do       -- 2
        foo();            -- 3
    end while;
    if g(x) then          -- 4
        foo();            -- 5
    else if h(x) then     -- 6
        bar();            -- 7
    else
        baz();            -- 8
    end if;
    y := x;               -- 9, 10
    return x;             -- 11
end;
```

The "resolution" of our positions doesn't have to be too exact (we don't care
about the position of expressions and subexpressions), each position represents,
not exactly a statement, but rather the _expression-bearing_ part of a
statement, so an assignment statement does not have one position but two: one
for the l-value and one for the right hand side.

Given two positions `a` and `b`, if `a < b` then `a` happened before `b`
(assuming all branches are taken).

Note that the position 0 is reserved for function parameters, so if we have a
linear value as a parameter the lifetime of that variable begins at 0.

## Data Structures

Two data structures are built up in the lifetime analysis pass: the table of
appearances and the table of lifetimes.

### Table of Appearances

The table of appearances maps the names of linear variables to:

1. The position where they are defined.

2. The loop context where they are defined.

3. The list of appearances, where each appearance is a triple `(pos, kind, ctx)`, where:

    1. `pos` is the position where the appearance, well, appears,

    2. `kind` is the type of appearance, one of `{Consume, Path, ReadBorrow,
       WriteBorrow}`, where `Consume` means a consumption appearance
       (e.g. `f(x)`), `Path` means the variable appears as the head of a path
       (e.g. `x.foo.bar`), `ReadBorrow` means the variable is borrowed read-only
       (e.g. `&x`) and `WriteBorrow` means the variable is borrowed
       read-and-write.

    3. `ctx` is the loop context of the appearance.

The concept of a loop context is best explained with an example:

```
let x: T := Make_T();
while ... do
    let y: U := Make_U();
    for i from 0 to n do
        while ... do
            consume_y(y);
        end while;
    end for;
end while;
consume_t(x);
```

Here, the loop context where `x` is defined is the empty list `[]`, and the loop
context where `x` is consumed is also the empty list `[]`. The loop context
where `y` is defined is `[While]`, since it is defined inside a while loop, and
the loop context where it is consumed is `[While, For, While]`, because it is
consumed inside a while loop that's inside a for loop that's inside a while
loop.

Basically the loop context is: how many levels of loop are you in. This is used
to verify that linear variables defined outside a loop are not consumed in the
body of an inner loop. If a variable `x` has the same loop context where it is
defined and where it is consumed, it's all good. If a linear variable has a loop
context `[l_0, ..., l_n]` with `n >= 0` but it is consumed in a loop context
`[l_0, ..., l_n, ..., l_m]` with `m > n`, that's an error. The above example
does not pass linearity checking: `y` is consumed potentially many times.

Given the following code:

```
let x: T := Make();
f(x.foo);
g(&x);
while cond() do
    h(&!x);
end while;
consume(x);
```

After annotating with position information:

```
let x: T := Make(); -- 1
f(x.foo);           -- 2
g(&x);              -- 3
while cond() do     -- 4
    h(&!x);         -- 5
end while;
consume(x);         -- 6
```

The table of appearances would look like this:

| Name | Position | Loop Ctx | Appearances |
| ---- | -------- | -------- | ----------- |
| `x`  | 1        | `[]`     | `[(2, Path, []), (3, ReadBorrow, []), (5, WriteBorrow, [While]), (5, Consume, [])` |

### Table of Lifetimes

The table of lifetimes maps regions to their start and end positions.

For example, in the following code:

```
let x: T := Make();
foo(&x);
let r: Reference[T, R] := &x;
bar(&ref);
consume(x);
```

Annotated with positions:

```
let x: T := Make();           -- 0
foo(&x);                      -- 1
let r: Reference[T, R] := &x; -- 2
bar(&ref);                    -- 3
consume(x);                   -- 4
```

The table would look like this:

| Region             | Start | End |
| ------------------ | ----- | --- |
| _Unnamed region 1_ | 1     | 1   |
| `R`                | 2     | 3   |

## Variable Registration Pass

We traverse the code depth-first and register all variables with a linear type. into the table of
appearances. For example:

```
function Foo(x: T): U is
    let y: U := f(x);
    while cond() do
        let z: U = copy(&y);
    end while;
    return y;
end;
```

Would result in this:

| Name | Position | Loop Ctx  | Appearances |
| ---- | -------- | --------- | ----------- |
| `x`  | 0        | `[]`      | `[]`        |
| `y`  | 1        | `[]`      | `[]`        |
| `z`  | 3        | `[While]` | `[]`        |

## Variable Appearances Pass

We traverse the code depth-first and register all variable appearances.

## Region Lifetimes Pass

[TODO]

## Rules Checking

When all the lifetime analysis passes are complete and the tables are filled in,
we can check the rules.

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
