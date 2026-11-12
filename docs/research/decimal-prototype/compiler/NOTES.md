> サブエージェントが試作の直後に書いた記録を、英語のまま置いたものである。日本語の要約と再実行の方法は `../README.md` にある。
> この記録の後で変わった点が3つある。`work/`、`fixtures/`、`verify-patch.sh`、`build.sh` はリポジトリに置いておらず、役目は `../run.sh` が引き継いだ。`puts` の衝突は、`embed-runtime.py` の書き換えではなく、`../runtime/decimal.c` の側で直した。`compiler-slice.patch` は実行時の C を含まない23ファイルの版に作り直した。

# Decimal[P, S] compiler slice: notes

This is a throwaway prototype for issue #19. It pushes the decided `Decimal[P, S]` design through the type checker and the code generator of the Zustral compiler at commit f601bcb. The code is a hack. The observations below are the deliverable.

Every statement is marked by how I know it. "Ran" means I executed it and saw the result. "Read" means I read the source and did not run it. "Not verified" means neither.

## What ran, and the results

| Check | Result |
|---|---|
| `run-tests.sh` against the worktree build | `pass 81 / fail 0` |
| `verify-patch.sh`: export HEAD f601bcb, `patch -p1 < compiler-slice.patch` (25 files), build, `run-tests.sh` | `pass 81 / fail 0` |
| `run-tests.sh` against an unpatched build of f601bcb (negative control) | `pass 2 / fail 79`. The two passes are the tests that use no Decimal. |
| The repository's end-to-end suite, `python3 test-programs/runner.py`, with the patched compiler | 284 PASS, 0 FAIL. Before the `puts` change described below it was 279 PASS, 5 FAIL. |
| `dune runtest` on the patched tree | exit 0 |
| `run-examples.sh` on the patched tree | exit 0 |
| Standard library tests, `make -C standard && ./standard/test_bin` | Fails with `Assertion failed (assertTrue)` after 94 passed assertions. An unpatched build of f601bcb fails the same way, with byte-identical output. The patch did not cause it. |
| menhir conflicts | 2 shift/reduce before the grammar change and 2 after |

Logs are in `work/logs/`. The C compiler was `cc` 13.3.0 on x86_64 Linux. I did not try clang.

Not run: nothing in this prototype was compiled for a platform without `__int128`. The experiment patch in `experiments/` was not run against the repository's end-to-end suite.

## How to rebuild

`make` fails in a worktree that sits inside another checkout. `dune build` walks up to the outermost `dune-project`, finds the parent checkout, and reports `No rule found for alias .claude/worktrees/.../default`. `build.sh <worktree>` runs `dune build --root .` instead. That script also holds the nix flags.

`embed-runtime.py <worktree> <runtime-dir>` rewrites the block between the marker comments in `lib/prelude.h` and `lib/prelude.c` from `decimal.h` and `decimal.c`. Run it again to swap the runtime, then rebuild.

## Status of each design point

| Design point | Status | Test names |
|---|---|---|
| Type `Decimal[P, S]`, literal arguments only, 1 <= P <= 38, 0 <= S <= P | Done | `type-*` |
| Free universe, `__int128` coefficient, signed | Done. `sizeof(Decimal[7, 2])` is 16. | `type-is-free`, `type-bounds` |
| Literal becomes a Decimal in a `let` and in an ascription, with the exact-fit check by value | Done | `literal-forms`, `literal-too-*`, `literal-ascription-misfit` |
| Literal in a `return` | Done | `literal-return`, `literal-return-misfit` |
| Exponent notation is a compile error | Done for `1.5e3`. `1e3` is already a parse error for every type, because the lexer requires a period in a float literal. | `literal-exponent` |
| `a + b`, `a - b`, same type, trap on overflow | Done | `add-sub`, `add-overflow`, `sub-overflow` |
| Different Decimal types in `+` and `-` are a compile error | Done | `add-different-types`, `sub-different-scale` |
| A literal operand of `+` and `-` takes the other operand's type, with the exact-fit check | Done | `add-sub`, `add-literal-misfit`, `add-literal-too-large` |
| Unary `-a` | Done with no code. The parser's `0 - x` and the literal rule are enough. | `negate` |
| `a * b` gives `Decimal[P1 + P2, S1 + S2]`, P1 + P2 > 38 is an error, a literal gets its smallest type | Done | `mul`, `mul-largest`, `mul-precision-over-38`, `mul-result-type-is-exact` |
| `a / b` is a compile error that points to `divide` | Partly. Done when an operand is a Decimal or a non-integer literal. Two integer literals escape it. See "Integer literal arithmetic is folded". | `div-operator`, `div-operator-literal`, `gap-integer-literal-division-folds` |
| `divide`, `round`, `exact`, `widen` take the result type from a `let`, a `return` or an ascription | Done | `divide-positions`, `round`, `exact`, `widen` |
| The same four with no context are a compile error | Done | `*-no-context*` |
| Traps: overflow, zero divisor, inexact `exact` | Done | `divide-by-zero`, `divide-overflow`, `round-overflow`, `exact-inexact`, `exact-overflow` |
| `widen` compile-time rule | Done | `widen-smaller-scale`, `widen-fewer-integer-digits` |
| `RoundingMode` with seven cases, the runtime takes the case index | Done, and it needed a compiler change outside the Decimal code. See "The C tag of a union case was not its index". | `rounding-mode-union`, `divide-modes` |
| Comparison, same type only, a literal takes the other side's type | Done | `compare*` |
| Output for tests | `debugPrintDecimal(x)`, a built-in. `printLn(x)` is not supported. | all |
| Stretch: `checkedAdd`, `checkedDivide` returning `Option[Decimal[..]]` | Partly. The results are right. Each call evaluates its operand expressions twice. | `stretch-checked*` |
| Stretch: `widen` and `exact` from integer types | Done | `stretch-from-integer`, `stretch-widen-integer-too-narrow`, `stretch-exact-integer-overflow` |
| Stretch: `round` from Float64 | Done for a Float64 expression. A bare literal such as `0.1` is read as a Decimal literal. | `stretch-round-from-float*` |

`divide-modes` tells all seven modes apart. It divides five pairs whose quotients at scale 2 differ across the modes, and the generator asserts that the seven expected rows are distinct.

## Files and functions touched

The patch changes 25 files, 976 insertions and 7 deletions. 352 of the inserted lines are the embedded runtime.

### Forced by the new type constructors

I added `DecimalTy of int * int` to `Type.ty`, `SDecimal of int * int` to `TypeStripping.stripped_ty`, and `MonoDecimal of int * int` to `MonoType.mono_ty`. A build with warning 8 switched to non-fatal listed these functions (Ran):

- `Type.type_string`, `Type.equal_ty`
- `TypeSystem.type_universe`, `is_numeric`, `is_comparable`, `type_variables`, `is_concrete`
- `TypeBindings.replace_variables`
- `TypeClasses.check_instance_argument_has_right_shape`, `overlapping_instances`, `is_type_local`
- `TypeMatch.match_type`
- `TypeStripping.strip_type'`
- `Monomorphize.monomorphize_ty`, `Monomorphize.mono_to_ty`
- `ExportInstantiation.transform_ty`
- `CodeGen.gen_type`, `CodeGen.gen_decl`

These are the predicted 17 plus `Monomorphize.monomorphize_ty`. The extra one comes from `SDecimal`. A type that carries P and S needs a constructor in the stripped type too.

Each of the 18 edits is one to nine lines and none needed thought, with these exceptions:

- `is_numeric` returns false for Decimal on purpose. Decimal arithmetic then cannot reach the `TrappingArithmetic` typeclass path.
- `is_comparable` and `is_concrete` have no callers anywhere in `lib/` (Ran: grep). Their answers for Decimal have no effect.
- `is_type_local` returns true, as it does for `Int32`. Any module may then declare an instance for a Decimal type.
- `CodeGen.gen_decl` and `ExportInstantiation.transform_ty` let a Decimal cross to foreign C code as `au_decimal_t`. Not verified. No test passes a Decimal to a foreign function or exports one.

### Forced by the grammar change

I reused `grammar-literal.patch` as it is: a `typespec_arg` rule, `Cst.ConcreteNatSpec` and `Stages.Ast.QNatSpec`. Three functions follow: `Qualifier.qualify_typespec`, `CstUtil.typespec_string` and `TypeParser.parse_type`.

`parse_type` turns `Decimal[<literal>, <literal>]` straight into `DecimalTy (p, s)` and rejects a literal argument anywhere else. So this design needs none of the other value-parameter machinery from the earlier spike: no `TyNat`, no `MonoNat`, no `NaturalUniverse`, and none of the seven universe functions. Forced functions in total: 17 + 1 + 3 = 21.

### Not forced, changed by choice

- `TypeCheckExpr.ml`, 437 inserted lines. Changed functions: `augment_expr` (the literal cases and the call case), `augment_comparison`, `augment_arithmetic`, `augment_function_call`, `augment_record_constructor`, `augment_union_constructor`. New functions: `decimal_literal_as`, `decimal_literal_minimal`, `coerce_decimal_literal_args`, `augment_decimal_arithmetic`, `augment_decimal_builtin`, and small helpers.
- `DecimalLit.ml`, new, 74 lines. It parses a literal's text and computes the coefficient and the smallest type with Zarith. It uses no floats.
- `Env.get_union_cases`, one line.
- `BuiltIn.pervasive_imports` and `builtin/Pervasive.aui` for `RoundingMode`.
- `prelude.h` and `prelude.c` for the runtime.

### Not touched

The typed AST (`Stages.Tast`, `Stages.Mtast`), `TypingPass`, `LinearityCheck`, `TypeReplace`, `TastUtil`, `MtastUtil`, `CRepr` and `CRenderer` are unchanged. The reason is a hack. Every Decimal operation, and every Decimal literal, becomes an existing `TEmbed` node, which is the node behind `@embed`. For example `a + b` becomes `TEmbed (DecimalTy (7, 2), "au_dec_add($1, $2, 7)", [a; b])`. The generated C is then `((au_decimal_t)(au_dec_add(x, y, 7)))`.

A real implementation would not build C text in the type checker. My reading of the code (Read, not run) is that a new typed-AST node costs a match arm in each of `TastUtil.get_type`, `TypeReplace`, `LinearityCheck`, `Monomorphize`, `TypingPass.is_constant`, `MtastUtil.get_type` and `CodeGen.gen_exp`.

I did not review the 81 wildcard matches from the earlier spike one by one. The ones my tests exercise behave correctly with no change: `TypeMatch.match_type_with_value` falls through to the strict match, `cast_arguments` adds no cast, and `augment_typecast` treats Decimal as neither integer nor float.

## Where the design was awkward in this compiler

### The expected type reaches only three kinds of position

`let x: T := e;` is not a separate mechanism. `AbstractionPass.let_reshape` rewrites it to an assignment of `e : T`, so a `let` annotation is an ascription (Read: `AbstractionPass.ml:173`). `return` passes the function's return type, and so does a constant declaration (Read: `TypingPass.ml:392` and `:602`). Those cases work with no new plumbing.

The expected type does not reach these positions (Ran, each has a test):

| Position | What happens | Test |
|---|---|---|
| Operand of `+`, `-`, `*` | `let g: Decimal[16, 9] := widen(a) + tax;` is a compile error. The user writes `(widen(a) : Decimal[16, 9]) + tax`. | `divide-no-context-operand` |
| Function argument | `show(divide(a, b, m))` is a compile error even though `show` declares `Decimal[9, 4]`. | `divide-no-context-argument` |
| Right-hand side of an assignment | `x := 2.5;` and `q := divide(a, b, m);` are compile errors. The user writes `x := 2.5 : Decimal[7, 2];`. | `gap-literal-in-assignment`, `gap-divide-in-assignment` |
| Branches of an `if` expression | `let x: Decimal[7, 2] := if c then 1.5 else 2.5;` is a compile error. | `gap-literal-in-if-expression` |
| Argument of a generic constructor or function | `let o: Option[Decimal[7, 2]] := Some(value => 1.23);` is a compile error. `T` is bound to Float64 first. | `gap-literal-under-generic-constructor` |

"Widen, then add" is the most common shape in money code, and it is the first case in the table. I think the design should decide this case on purpose.

I measured the cost of closing the first two rows. `experiments/expected-type-flow.patch` adds about 50 lines to `TypeCheckExpr.ml` on top of the slice. With it, `let g: Decimal[16, 9] := widen(a) + tax;` compiles and prints `32.458762500`, `debugPrintDecimal(widen(a) + tax)` takes the type from the other operand, and `show(divide(a, b, m))` compiles. `widen(a) + widen(a)` with no context stays an error, and so does `widen(a) * b`, because `*` cannot name its operand types. `run-tests.sh` against that build gives `pass 78 / fail 2`, and the two failures are the two tests that assert the error. The observed output is in `experiments/expected-type-flow.observed.txt`. The experiment is not part of `compiler-slice.patch`, because it changes what the decided design calls an error.

The assignment row is different in kind. `LiftControlPass` moves the right-hand side of every assignment into a fresh temporary before type checking, and the temporary is typed with no expected type (Read: `LiftControlPass.ml:19`). The pass runs on untyped syntax and cannot know the variable's type. Integers already have this problem. `var i: Int64 := 0; i := 5;` fails with "Expected a value of type `Int64`, but got a value of type `Int32`" (Ran on the patched build. The patch does not touch that path).

### The built-ins cannot be functions

`divide`, `round`, `exact`, `widen`, `checkedAdd`, `checkedDivide` and `debugPrintDecimal` are names that the type checker intercepts in `augment_expr` before it looks up a callable. They cannot be declared in `Pervasive`, because their result type depends on P and S as numbers and a function body has no way to read a type argument as a number. Consequences, all Ran:

- A function or variable of the program with the same name wins, silently (`builtin-name-taken-by-program`). The design does not say whether these names are reserved. `round` is a likely name in user code.
- They accept positional arguments only. `divide(a => x, b => y, mode => m)` is an error in the prototype.
- They cannot be passed as function values or imported under another name. Not verified by a test. It follows from how they are intercepted.

### The C tag of a union case was not its index

The design says the runtime takes the case index of `RoundingMode` as an int. At HEAD the C tag enum of a union lists the cases in reverse declaration order. The first generated enum was `mono_14_tag_TowardLesser, ..., mono_14_tag_NearestEven`, so `NearestEven()` rounded as `TowardLesser` (Ran: `divide-modes` failed until the change below).

The cause is in `Env.get_union_cases`. It filters the declaration list, and the environment prepends each new declaration.

I changed `get_union_cases` to return the cases sorted by declaration id. The change renumbers the tags of every union in every program. The repository's end-to-end suite still passes with it (Ran: 284 of 284). The tag values are symbolic everywhere else that I found (Read: `Entrypoint.ml` uses the enum names).

The spec has to pick one of two things: guarantee that a union's C tag is its declaration index, or stop passing the tag as a number.

### Pervasive names win over the program's own names

`RoundingMode` and its seven cases are added to the names that every module imports from `Austral.Pervasive`. An imported name wins over a declaration in the program. A program with its own `union Direction` that has a case `TowardZero` gets "Cannot cast to `Direction` from `RoundingMode`" for `let d: Direction := TowardZero();` (Ran: `gap-rounding-mode-case-name-taken`). A program function named `NearestEven` is hidden the same way (Ran: probe `name-collision-function`).

The design adds eight names that programs can no longer use, and the error does not say why. The existing pervasive names, such as `Some` and `None`, have the same property. I did not test that.

### Integer literal arithmetic is folded before type checking

`AbstractionPass.try_folding` evaluates arithmetic on integer literals while it builds the AST. `4 / 3` reaches the type checker as the literal `1`. Two effects (Ran):

- `let x: Decimal[7, 2] := 4 / 3;` compiles and prints `1.00`. The rule "`/` on Decimal is a compile error" never sees it (`gap-integer-literal-division-folds`).
- `let b: Decimal[7, 2] := 1 + 2;` works only because of the folding (`literal-arithmetic`).

Float literals are not folded, so `4.0 / 3` in a Decimal position does reach my code and is rejected.

### A literal's original form is lost before the type checker

Hex, binary, octal and character literals become decimal integer text in the parser. `let x: Decimal[7, 2] := 'a';` prints `97.00` and `#xFF` prints `255.00` (Ran: `gap-other-literal-forms`). The type checker cannot reject them without a parser change.

The lexer also accepts `1.` as a float literal, and it reads a sign as part of the literal, so `a -1` is the identifier `a` followed by the literal `-1`. Both are old behaviour. `1.` works as a Decimal literal (Ran: probe `trailing-period`).

### The result type of `*` must be written out by hand

Austral has no type inference for `let`. The user must compute P1 + P2 and S1 + S2 and write `let total: Decimal[12, 5] := price * quantity;`. The literal rule makes the answer depend on the literal's digits: `price * 2` is `Decimal[8, 2]` and `price * 10` is `Decimal[9, 2]`.

A wrong annotation gives "Cannot cast to `Decimal[9, 2]` from `Decimal[8, 2]`" (Ran: `mul-result-type-is-exact`). The message names the real type, which helps. It says "cast" because a `let` is an ascription.

### The embedded runtime's `puts` declaration broke existing programs

`decimal.c` declares `extern int puts(const char *text);` inside `au_dec_debug_print`. Austral programs that import `puts` get a local `extern au_int32_t puts(au_nat8_t* str);` in the same C file, and gcc rejects the pair as conflicting types. Five of the repository's end-to-end tests import `puts` and failed (Ran).

`embed-runtime.py` now rewrites that one function to call the prelude's `au_printf("%s\n", buf)`. This is the only change I made to the runtime text. The assertion in the script fails if `decimal.c` stops containing the original lines. The parent may prefer to make the same change in `decimal.c`.

`zd_to_double` declares `strtod` the same way. A program that imports `strtod` with another signature would hit the same error. Not verified. I left it alone.

Only `prelude.c` is prepended to generated C (Read: `Compiler.ml:146`). `prelude.h` got the header block because the brief asked for it. Nothing in this prototype reads it.

### Smaller points

- Checked forms evaluate their operands twice. `checkedAdd(a, b)` becomes `zd_add(a, b, P, &(au_decimal_t){0}) ? Some(au_dec_add(a, b, P)) : None()`. The typed AST has no expression that binds a temporary, and an `@embed` string cannot name the `Option` struct, whose C name is assigned later. A real version needs a statement-level lowering or a runtime call that returns the pair.
- A literal costs a function call. `1.23` becomes `au_dec_lit(0, 0ULL, 123ULL)`. Module constants still work, because the compiler emits a constant as a `#define` (Ran: `literal-constant-declaration`). It is not a C constant expression.
- Generic types are copied per (P, S), as the earlier report predicted. `Option[Decimal[3, 2]]` and `Option[Decimal[9, 4]]` are the separate structs `mono_13` and `mono_15` in the C for `stretch-checked` (Ran).
- A typeclass instance works per concrete type. `instance Show(Decimal[9, 2])` and `instance Show(Decimal[12, 5])` live together and dispatch correctly, across a module interface (Ran: `interface-module`). No instance can cover all Decimal types, so `printLn` cannot get one in the language. `printLn(d)` fails with "does not implement the typeclass `Printable`" (Ran: probe `printLn`).
- Interface and body agree on Decimal types. `interface-module` declares a constant, a record, two functions and two instances with Decimal types in `Money.aui` and defines them in `Money.aum` (Ran).
- `Decimal[07, 02]` is accepted as `Decimal[7, 2]`. `Decimal[1'0, 2]` is rejected, because the digit separator is not stripped in type position. `Decimal[#x10, 2]` is a parse error. `Decimal[5, -1]` reaches `parse_type` as the literal `-1` and is rejected there (Ran: probes and `type-scale-negative`).

## Choices the design did not make

For each, the prototype does what is listed. Each one is a question for the spec.

1. A literal operand of `divide`, `round`, `exact`, `widen` or `checkedDivide` gets the smallest Decimal type that holds it, as for `*`. `divide(a, 8, mode)` and `divide(1, b, mode)` work (Ran: `divide-positions`).
2. `round(0.1, mode)` reads `0.1` as a Decimal literal and gives `0.100000000000000000` at scale 18. `round(0.1 : Float64, mode)` gives `0.100000000000000006` (Ran: probe `round-float-literal`). The same text gives two different values, and the design should say which one a bare literal means.
3. Two literal operands. For `+` and `-`, both take the expected type when the position has one: `let a: Decimal[7, 2] := 1.5 + 2.25;` gives `3.75`. For `*`, each gets its smallest type, so `1.5 * 2` is `Decimal[3, 1]`. With no expected Decimal type, the old Int32 and Float64 rules apply (Ran: `literal-arithmetic`).
4. A literal argument. The design names `let` and ascription as the places where a literal becomes a Decimal. The prototype also accepts a literal as an argument of a function, a record constructor or a union constructor when the declared parameter or slot type is a Decimal: `show(1.23)` and `Price(amount => 12.5)` (Ran: `literal-argument`). It does this after the argument is checked, by rewriting the literal node, whose text is still available. Method calls and function-pointer calls do not get this. Not verified what they report.
5. `checkedAdd` requires an `Option[Decimal[P, S]]` from the position and checks both operands against that type. Its result type is already fixed by its operands, so the context adds nothing here. The design could let `checkedAdd` work with no context.
6. `widen` from an integer type uses these digit counts: 3 for 8-bit, 5 for 16-bit, 10 for 32-bit, 19 for Int64, and 20 for Nat64, Index and ByteSize. `widen(i)` from Int32 to `Decimal[9, 0]` is a compile error (Ran: `stretch-widen-integer-too-narrow`).
7. `widen` to the same type is allowed. `round` and `exact` to a larger scale are allowed (Ran: `widen`, `round`, `exact`).
8. A literal that needs more than 38 digits has no smallest type and is a compile error (Ran: probe `mul-39-digit-literal`).
9. A constant declaration and a `return` both supply the result type of a built-in, in addition to `let` and ascription.

## Things that surprised me

- Unary minus needed no code at all.
- No type-level arithmetic and no way to read a type argument inside a function body were needed. The type checker computes P1 + P2 in OCaml, and each built-in call writes P and S into the C call as integer constants. The two heaviest items of the earlier report do not arise when the arguments are literals only.
- The `#16` rule is never reached. The Decimal branch in `augment_arithmetic` runs before the "one operand is a constant" rule, and no `TCast` is ever built for a Decimal (Ran: `add-literal-too-large` and `add-literal-misfit` are compile errors).
- The compiler does not check that the operands of a comparison are comparable. `augment_comparison` only matches the two types. Decimal comparison compiles to a C comparison of two `__int128` values.
- The command-line tool prints its usage text and exits 0 for an unknown command. A typo in my test runner made one test "compile" with no output file.
- The compiler leaves `austral_XXXXXX.c` in `$TMPDIR` and writes `error.html` and `calltree.html` into the current directory. `run-tests.sh` runs each compile in its own temporary directory for that reason. My earlier runs left files named `austral_*.c` in `/tmp`. I did not delete them, because other sessions may own some of them.

## Layout of this directory

- `compiler-slice.patch` is `git diff HEAD` of the worktree. It holds compiler and prelude changes only.
- `tests/` holds 81 programs: 28 with `.expected`, 44 with `.error` and 9 with `.trap`. `tests/modules/` holds the two-file module that `interface-module` compiles with. Tests named `gap-*` pin down behaviour that falls short of the design or that the design does not cover. They pass when the prototype behaves as described in these notes.
- `run-tests.sh <austral>` runs the tests.
- `verify-patch.sh <checkout>` exports HEAD, applies the patch, builds, and runs the tests.
- `build.sh`, `embed-runtime.py` are described above.
- `experiments/` holds the expected-type experiment and its observed output.
- `work/gen_tests.py` generates `tests/` from `fixtures/`. The expected values in it were worked out from the design by hand before the first run. That first run failed 5 of 76 tests, and two of the causes are findings above: the tag order and the assignment position.
- `work/probe.py` holds 41 exploratory programs. Their output with the final build is in `work/logs/probe.observed.txt`.
