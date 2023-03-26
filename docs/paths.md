# Paths, L-Values, and Reference Transforms

This document describes the syntax and semantics of paths, l-values, and reference transforms.

## Paths

A _path expression_ is used to access nested values.

### Syntax

A path expression is:

1. An expression called the _head_,
2. Followed by one or more _path elements_, each of which is one of:
  1. A _slot accessor_, `.name`, where `name` is slot name.
  2. A _reference-slot accessor_, `->name`, where `name` is a slot name.
  3. An _array index_, `[i]`, where `i` is an expression of type `Index`.

### Semantics

1. The type of a path is the type of the nested value it accesses to.
1. **Rule P1:** the head of a path cannot be a linearly-typed expression.
  1. Rationale: if we had something like `f().x`, then the linear return value
     of `f()` would be marked consumed (by appearing at the head of a path),
     without it actually being destroyed anywhere.
  1. **Rule P1.1**: the exception is if the head of a path is a variable, in
     which case, its appearance doesn't count as a consumption. For example, if
     `pos` is linear, then doing `pos.lat` (where `lat` is a slot of type
     `Float32`) is perfectly safe. It is far less onerous to do `pos.lat` than
     to ask the user to taka reference to `pos`, transform it into a reference
     to `lat`, and dereference that.
 1. **Rule P2:** as a corollary of the above exception , the type of a path must
    be in the `Free` universe.
    1. Rationale: if paths could end in linear values, then we could duplicate them, like so:
    ```austral
    let x1: Bar := foo.bar; -- foo not consumed
    let x2: Bar := foo.bar; -- bar loaded twice
    ```
1. **Rule P3:** the head of a path expression must be a user-defined type, a
   reference, or a fixed array.
   1. Rationale: nothing else contains nested values.

The type-checking rules for path elements are:

1. `.name` needs the expression on the left to be a record type, its type is the
   type of `name`.
2. `->name` needs the expression on the left to be a read-reference or
   write-reference to a record type, its type is the type of `name`.
1. `[i]` needs the expression on the left to be a fixed array, its type is the
   type of the array's elements.

### Examples

```
pos.lat
foo.bar->baz[i]
```

## Reference Transforms

A _reference transform expression_ transforms a reference to a value into a
reference to a more nested value.

### Syntax

Reference transforms have a similar syntax to paths, but they are wrapped in `&(...)`.

The syntax of a reference transform expression is:

1. Inside `&(...)`:
   1. An expression, called the _head_,
   1. Followed by one or more _transform elements_, each of which is one of:
      1. A _reference-slot accessor_, `->name`, where `name` is a slot name.
      1. An _array index_, `[i]`, where `i` is an expression of type `Index`.

Why one or more? Because `&(foo)` would be a no-op.

### Semantics

1. **Rule RT1:** the type of the head must be either a read reference or a write
   reference.
1. **Rule RT2:** the type of a reference transform is a read reference if the
   head is a read reference, and a write reference if the head is a write
   reference.
1. **Rule RT3:** transforming a write reference consumes it.
1. **Rule RT4:** you can't take a mutable reference into a static array.

The type-checking rules are:

1. `&(foo->x)`: if `foo` is a reference to a record, then this is a reference to
   the slot `x` in `foo`.
1. `&(foo[i])`: if `foo` is a reference to a fixed array, then this a reference
   to the `i`-th element of the array.

### Examples

```
&(foo->lat->float)
&(x->y[idx]->z)
```

## L-Values

L-values are the expressions left-hand side of an assignment. The "L" is for
"left".

### Syntax

An L-value is either:

1.  A variable called the _head_, (this is the "simple lvalue" case)
    1. Followed by zero or more _l-value elements_, which are one of:
       1. A _slot accessor_, `.name`, where `name` is slot name.
       1. A _reference-slot accessor_, `->name`, where `name` is a slot name.
1. The dereference operator applied to an expression of write-reference type
   (the "reference lvalue" case).

### Semantics

1. The type of an L-value is the type if it was evaluated as an expression.
1. **Rule L1:** the type of an L-value must match the type on the right-hand
   side of the assignment.
1. **Rule L2:** the type of an L-value must be in the `Free` universe:
   1. Rationale: consider:
      ```austral
      -- `Foo` is a linear type
      let l: Foo := f();
      -- The original value of `l` is overwritten.
      l := g();
      -- The return value of `f()` leaks.
      consume(l);
      ```
      If we're allowed to overwrite linear values, their original contents leak.
1. **Rule L3:** the write reference is consumed.

### Examples

```
a := 10;
a.b.c := 10;
!(ref) := x;
!(&(x->y->z)) := 20;
```
