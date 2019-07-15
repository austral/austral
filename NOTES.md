# Implementation Notes

## General Principles

- Separate intermediate representations from passes. Define an SML module
  (`.sig` and `.sml` file) for the representation, another for the pass.

## Intermediate Representations

Code travels down the following intermediate representations:

- Syntax (module `Syntax`): this is the output straight from the parser, the basic abstract
  syntax tree.
- Ordered declarations AST.
- Resolved declarations AST.
- Typed declarations AST.

## Linear API

### Pointers

- `function allocate<T>(value: T): Optional(Pointer(T))`

  Allocate memory to hold the value. Optional::None indicates allocation failure.

  In a no-runtime implementation, this always returns Optional::None.

- `function deallocate<T>(p: Pointer(T)): T`

  Deallocation and dereferencing are the same.

- `function load<T: Type*>(p: Pointer(T)): (T, Pointer(T))`

  If the pointed-to type is unrestricted, we can dereference it freely.

- `function swap<T>(p: Pointer(T), value: T): (T, Pointer(T))`

  Replace the pointers of a linear pointer, returning the old contents.

- `function store<T: Type*>(p: Pointer(T), value: T): Pointer(T)`

  If the pointed-to type is unrestricted, we can write to it freely.

### Arrays

- `function empty<T>(): Optional(Array(T))`

  Allocates the empty array. Optiona::None indicates allocation failure.

  In a no-runtime implementation, this always returns Optional::None.

- `function iota<T: Type*>(value: T, size: Size): Optional(Array(T))`

  If the contained type is unrestricted, we can preallocate an array with that value.

  The name comes from APL (by way of Scheme or C++).

- `function replace<T>(array: Array(T), index: Size, value: T): (T, Array(T))`

  If the contained type is linear, we replace a value at an index, returning the old value.

- `function append<T>(array: Array(T), value: T): Optional(Array(T))`

  Add an element to the end of the array, expanding the capacity if necessary.

  If the return value is Optional::None, this indicates allocation failure.

### Unsafe Memory

Built-in module: `Austral.Memory`

- `type MemoryCapability : Type1`

  This is a linear capability for memory access.

- `function acquire(world: World): (MemoryCapability, World)`

  Obtain permission to access memory.

- `function waive(cap: MemoryCapability): ()`

  Waive permission to access memory.

- `type Address`

  The type of memory addresses.

- `constant null`

  The null address.

- `function loadSigned8(cap: MemoryCapability, addr: Address): (cap, Signed8)`

  Dereference a signed 8 bit integer from an address.

# Language Notes

## Syntax

EBNF quick guide:

Definition is `=`, terminates with `;`. Concatenation is `,`. Alternation is
`|`. Optional is `[...]`. Repetition (zero or more) is `{...}`.

For brevity, we use `<...>` to mean comma-separated repetition, e.g. the empty
string, `A`, `A,B`, `A,B,C`, without a trailing comma.

Non-terminals are in `PascalCase`. Terminals are `lower case`. Whitespace is
implied.

```
(* Declarations *)
Module = [docstring], "module", module name, {Import}, {Definition};
Import = "from", module name, "import", identifier, ["as" identifier];
Definition = RecordDef | UnionDef | FunctionDef | TypeClassDef | ClassInstanceDef;

(* Type definitions *)
RecordDef = [docstring], [TypeVis], "record", identifier, [TypeParams], "{", <Slot>, "}";
UnionDef = [docstring], [TypeVis], "union", identifier, [TypeParams], "{", <Case>, "}";

TypeVis = "opaque" | "public";
TypeParams = "(", <identifier, [":", Universe"]>, "}", [":", "Universe"];
Universe = "Type1" | "Type*";

Slot = identifier, ":", TypeSpec, [docstring];
Case = identifier, [":", TypeSpec], [docstring];

TypeSpec = identifier | "(", <TypeSpec>, ")" | identifier, "(", <TypeSpec>, ")";

(* Function definitions *)
FunctionDef = [docstring], ["public"], "function", identifier, FuncTypeParams, FuncSignature, Block;
FuncTypeParams = "<", <identifier, [":", Universe | "*"]>, ">";
FuncSignature = "(", <Param>, ")", ":", TypeSpec;
Param = identifier, ":", TypeSpec, [docstring];
Block = "{", {Statement}, "}";

TypeClassDef = [docstring], "class", ident, "(", ident, [":", Universe], ")", ["extends", <ident>], "{", {MethodDecl}, "}";
MethodDecl = [docstring], "function", ident, FuncSignature, ";";

ClassInstanceDef = [docstring], "instance", ident, "for", InstanceArgument, "{", {MethodDef}, "}";
InstanceArgument = ident | ident, "(", <ident>, ")";
MethodDef = [docstring], "function", identifier, FuncSignature, Block;

(* Statements *)
Statement = BindingDeclaration
          | Assignment
          | IfStatement
          | CaseStatement
          | ForLoop
          | WhileLoop
          | "abort", ";"
          | "return", Expression, ";";

BindingDeclaration = ["let" | "var"], Binding, [":", TypeSpec], ":=", Expression, ";";
Binding = identifier | "(", <Binding>, ")" | "{", identifier, "}";
Assignment = Place, ":=", Expression, ";";
Place = identifier;
IfStatement = "if", Expression, Block, {"else if", Block}, ["else", Block];
CaseStatement = "case", Expression, "{", { identifier, Binding, Block }, "}";
ForLoop = "for", identifier, [":", TypeSpec], "from", Expression, "to", Expression, Block;
WhileLoop = "while", Expression, Block;

(* Expressions *)
Expression = Constant
           | TupleLiteral
           | ArithExpr
           | ComparisonExpr
           | BooleanExpr
           | Variable
           | Funcall
           | IfExpression
           | CaseExpression
           | UnionConstructor;
PExpression = Constant | Variable | "(", Expression, ")" | Funcall;

Constant = "nil" | "true" | "false" | integer constant | float constant | string constant;
TupleLiteral = "(", <Expression>, ")";
Variable = "identifier;
Funcall = identifier, "(", <Expression>, ")";
IfExpression = "if", Expression, "then", Expression, "else", Expression;
CaseExpression = "case", Expression, "of", {identifier, Binding, "=>", Expression};
UnionConstructor = identifier, "::", identifier, "(", <Expression>, ")";

ComparisonExpr = PExpression, ("=", "<", "<=", ">", ">=", "/="), PExpression;
BooleanExpr = "not" PExpression
            | PExpression, {"and", PExpression}
            | PExpression, {"or", PExpression};

(* Arithmetic expressions *)
ArithExpr = PExpression, {("+" | "-" | "*" | "/" | "^"), PExpression}
          | "-", Expression;

(* Terminals *)

(* Identifiers of various kinds *)
module name = module identifier, { ".", module identifier };
module identifier = letter, { letter | digit }
identifier = letter, { letter | digit | symbol };

(* Documentation strings *)
docstring = "`", { any character - "`" | "\`" } ,"`";

(* Literals *)
digits = digit, { digit | "_" };
integer constant = digits;
float constant = digits, ".", digits, ["e", ["+", "-"], integer constant];
string constant = '"', { any character - '"' | '\"' }, '"';

(* Auxiliary *)
letter = uppercase | lowercase;
uppercase = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
          | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
          | "U" | "V" | "W" | "X" | "Y" | "Z"
lowercase = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
          | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t"
          | "u" | "v" | "w" | "x" | "y" | "z"
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
symbol = "$" | "?" | "'"
```
