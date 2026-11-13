# Inventories for R21, R24 and R25

Source: the read-only copy of the compiler at commit c7af9a6 (`lib/` and `cli/`). `lib/BuiltInModules.ml` is generated and is left out. All line numbers are those of that copy. Nothing was built. The tables come from ripgrep and from reading the code around each hit; the one thing that was run is the already-built compiler binary, for the five probes in 2.3. The rows were typed by hand into a script (`inventory-tools/gen_inventory.py` next to this file) that checks every file:line against the source and computes the totals.

## 1. Environment lookups (for R21)

### 1.1 What `lib/Env.mli` exports

Twelve lookup functions return `option`. The plan says 13; `Env.mli` was last changed by R7 (e2afef8).

`get_module_by_id`, `get_module_by_name`, `get_decl_by_id`, `get_decl_by_name`, `get_method_from_typeclass_id_and_name`, `get_instance_method_from_instance_id_and_method_name`, `get_instance_method`, `get_callable`, `get_type_monomorph`, `get_function_monomorph`, `get_monomorph`, `get_variable`.

`get_decl_by_name` is not a pure `option` function: it raises `Errors.unknown_module` (Env.ml:125) when the module part of the name does not exist. `get_callable` and `get_variable` inherit that, and `get_callable` also raises a bare `err "No union with ID"` (Env.ml:305).

Two lookups return a value or raise: `module_name_from_id` (Env.ml:84) and `visible_instances` (Env.ml:344). Five return a list and cannot fail: `module_instances`, `module_public_instances`, `get_union_cases`, `get_uninstantiated_monomorphs`, `get_export_functions`. Six update functions look an id up and raise `internal_err` when it is missing: `store_function_body`, `store_method_body` and the four `store_*_monomorph_definition`.

### 1.2 Calls of the `option` lookups outside `lib/Env.ml`

43 calls in 11 files. There is none in `cli/`. "Key" says where the key comes from:

- **id**: an id that an earlier environment call returned.
- **id + member name**: an id together with a method name.
- **user-written name**: a name taken from the source text (or the command line) at this point, not checked before.
- **name, not directly user-written**: a name the compiler fixed itself, or a name that an earlier pass already resolved and that is looked up again (the name inside a `NamedType`, a module name in an import map).

| file:line | function called | key | where the key comes from | `None` becomes | detail |
|---|---|---|---|---|---|
| lib/CodeGen.ml:441 | `get_monomorph` | id | `mono_id` from an `MFunctionMonomorph` node | internal_err | `\| _ ->` at :452 also covers a monomorph that is not `MonoFunction` |
| lib/CodeGen.ml:443 | `get_decl_by_id` | id | `function_id` stored in the monomorph | internal_err | `\| _ ->` at :450 also covers a decl that is not `Function` |
| lib/CodeGen.ml:445 | `get_module_by_id` | id | `mod_id` stored in the decl | internal_err | :448 |
| lib/CodeGen.ml:468 | `get_monomorph` | id | `mono_id` | internal_err | :472 (`get_mono_or_die`) |
| lib/CodeGen.ml:475 | `get_decl_by_id` | id | `decl_id` stored in a monomorph | internal_err | :483; a decl with no name (an instance) is a second `internal_err` at :481 |
| lib/Entrypoint.ml:79 | `get_decl_by_name` | user-written name | entrypoint name from the command line | named user error | `Errors.no_such_function`; wrong kind is `Errors.not_function` (:107) |
| lib/Entrypoint.ml:162 | `get_decl_by_name` | name, not directly user-written | compiler-fixed name `Austral.Pervasive.RootCapability` | internal_err | `\| _ ->` at :169 also covers a decl that is not `Record` |
| lib/Entrypoint.ml:164 | `get_type_monomorph` | id | `decl_id` of the record + empty bindings | internal_err | :168 |
| lib/Entrypoint.ml:176 | `get_decl_by_name` | name, not directly user-written | compiler-fixed name `Austral.Pervasive.ExitCode` | internal_err | `\| _ ->` at :183 also covers a decl that is not `Union` |
| lib/Entrypoint.ml:178 | `get_type_monomorph` | id | `decl_id` of the union + empty bindings | internal_err | :182 |
| lib/EnvExtras.ml:15 | `get_module_by_id` | id | `mod_id` (wrapper `get_module_or_die`, used only inside EnvExtras) | internal_err | :17 |
| lib/EnvExtras.ml:24 | `get_decl_by_name` | name, not directly user-written | wrapper `get_decl_by_name_or_die`; its one caller (TypeClasses.ml:260) passes the name inside a resolved `NamedType` | internal_err | :27 |
| lib/EnvExtras.ml:32 | `get_decl_by_id` | id | `decl_id` (wrapper `get_decl_sident_or_die`; callers TypeCheckExpr.ml:396, :423 pass a typeclass id) | internal_err | :40; a decl with no name is a bare `err "decl has no name"` at :38 |
| lib/EnvExtras.ml:45 | `get_decl_by_id` | id | `decl_id` (wrapper `get_decl_name_or_die`; exported, but no caller in `lib/` or `cli/`) | internal_err | :53; a decl with no name is a bare `err` at :51 |
| lib/ExportInstantiation.ml:27 | `get_decl_by_id` | id | `decl_id` from `get_export_functions` | internal_err | `\| _ ->` at :34 also covers a decl that is not `Function` |
| lib/ExportInstantiation.ml:29 | `get_function_monomorph` | id | `decl_id` + empty bindings | internal_err | :33 |
| lib/ExportInstantiation.ml:38 | `get_decl_by_id` | id | `decl_id` from `get_export_functions` | internal_err | `\| _ ->` at :41 also covers a decl that is not `Function` |
| lib/ExtractionPass.ml:221 | `get_module_by_name` | name, not directly user-written | module names read back from the already-resolved import maps (the user wrote them in `import`) | internal_err | :223 |
| lib/ExtractionPass.ml:483 | `get_decl_by_name` | user-written name | typeclass name in an `instance` declaration | named user error | `Errors.unknown_typeclass` (:491); wrong kind is `Errors.instance_for_non_typeclass` (:489) |
| lib/ExtractionPass.ml:523 | `get_method_from_typeclass_id_and_name` | id + member name | typeclass id + method name the user wrote in the instance | named user error | `Errors.typeclass_no_method` (`\| _ ->` at :526) |
| lib/ImportResolution.ml:84 | `get_decl_by_name` | user-written name | imported symbol | named user error | `Errors.unresolved` (:97). A missing *module* is raised inside Env (`Errors.unknown_module`, Env.ml:125) |
| lib/ImportResolution.ml:117 | `get_module_by_name` | user-written name | module name of an import list | normal case | returns `[]` (no instances to import) |
| lib/Monomorphize.ml:81 | `get_decl_by_name` | name, not directly user-written | name inside a stripped type (`SNamedType`), i.e. a type the type checker already resolved | internal_err | :99; a decl that is not `Record`/`Union` is a second `internal_err` at :97 |
| lib/Monomorphize.ml:225 | `get_instance_method_from_instance_id_and_method_name` | id + member name | instance id + method name kept in the typed tree (`TVarMethodCall`) | internal_err | :227 |
| lib/Monomorphize.ml:659 | `get_decl_by_id` | id | `type_id` of a record monomorph | internal_err | `\| _ ->` at :662 also covers not-a-`Record` |
| lib/Monomorphize.ml:666 | `get_decl_by_id` | id | `type_id` of a union monomorph | internal_err | `\| _ ->` at :669 also covers not-a-`Union` |
| lib/Monomorphize.ml:684 | `get_decl_by_id` | id | `function_id` of a function monomorph | internal_err | `\| _ ->` at :691 also covers not-a-`Function` |
| lib/Monomorphize.ml:695 | `get_instance_method` | id | `ins_meth_id` of a method monomorph | internal_err | :707 |
| lib/Monomorphize.ml:697 | `get_decl_by_id` | id | `instance_id` stored in the instance method record | internal_err | `\| _ ->` at :704 also covers not-an-`Instance` |
| lib/Monomorphize.ml:778 | `get_decl_by_id` | id | `decl_id` from `get_export_functions` | internal_err | `\| _ ->` at :782 also covers not-a-`Function` |
| lib/TypeCheckExpr.ml:120 | `get_decl_by_name` | name, not directly user-written | name inside the `NamedType` of an expression (callers :689, :750) | bare err | `err "No record with this name: ..."` (:127); wrong kind is `Errors.path_not_record` (:125) |
| lib/TypeCheckExpr.ml:225 | `get_callable` | user-written name | callee name in a call expression | named user error | `Errors.unknown_name ~kind:"callable"` |
| lib/TypeCheckExpr.ml:268 | `get_decl_by_id` | id | `union_id` carried by the `UnionConstructor` callable | internal_err | `\| _ ->` at :272 also covers not-a-`Union` |
| lib/TypeCheckExpr.ml:277 | `get_decl_by_id` | id | `typeclass_id` carried by the `MethodCallable` callable | internal_err | `\| _ ->` at :280 also covers not-a-`TypeClass` |
| lib/TypeCheckExpr.ml:436 | `get_instance_method_from_instance_id_and_method_name` | id + member name | id of the instance just resolved + the method name the user wrote in the call | internal_err | :438 |
| lib/TypeCheckExpr.ml:551 | `get_decl_by_name` | user-written name | identifier used as a variable | normal case | `\| _ ->` (:569): not a function, so try `get_variable` |
| lib/TypeCheckExpr.ml:570 | `get_variable` | user-written name | identifier used as a variable | named user error | `Errors.unknown_name ~kind:"variable"` |
| lib/TypeMatch.ml:207 | `get_decl_by_name` | user-written name | typeclass name the user wrote in a type parameter constraint; it is stored unchecked and looked up when the generic function is called | internal_err | :218 (the message says the extraction pass should have validated it; it does not, see 2.3); wrong kind is a second `internal_err` at :216 |
| lib/TypeMatch.ml:222 | `get_module_by_name` | name, not directly user-written | name of the module being compiled (from the context, or saved in `TVarMethodCall`) | Option.get | `Option.get` raises `Invalid_argument` on `None` |
| lib/TypeParser.ml:215 | `get_decl_by_name` | user-written name | type name in a type specifier | normal case | returns `None`; the caller `parse_user_defined_type` (:289) turns it into `Errors.unresolved_type` |
| lib/TypingPass.ml:52 | `get_decl_by_name` | name, not directly user-written | name inside the `NamedType` of a destructured value (caller :158); copy of TypeCheckExpr.ml:119 | bare err | `err "No record with this name: ..."` (:59); wrong kind is `Errors.path_not_record` (:57) |
| lib/TypingPass.ml:79 | `get_decl_by_name` | user-written name | name of a new local variable, qualified with the current module | normal case | `None` is the success case; `Some _` is `Errors.var_collides_with_decl` |
| lib/TypingPass.ml:467 | `get_decl_by_name` | name, not directly user-written | name inside the `NamedType` of a `case` scrutinee | named user error | `Errors.case_non_union` (`\| _ ->` at :479, shared with not-a-`Union`) |

Totals by what the caller does with `None`:

| `None` becomes | count |
|---|---|
| internal_err | 29 |
| named user error | 7 |
| bare err | 2 |
| Option.get | 1 |
| normal case | 4 |
| **total** | 43 |

The plan, written before R7, counts 27 / 8 / 4 / 1 and 4 unclassified: 44 calls of 13 functions. Which rows it classified differently could not be reconstructed.

Totals by key:

| key | count |
|---|---|
| id | 21 |
| id + member name | 3 |
| user-written name | 10 |
| name, not directly user-written | 9 |
| **total** | 43 |

Key against `None` handling:

| key | internal_err | named user error | bare err | Option.get | normal case | total |
|---|---|---|---|---|---|---|
| id | 21 |  |  |  |  | 21 |
| id + member name | 2 | 1 |  |  |  | 3 |
| user-written name | 1 | 5 |  |  | 4 | 10 |
| name, not directly user-written | 5 | 1 | 2 | 1 |  | 9 |

Totals by function called:

| function | count |
|---|---|
| `get_decl_by_name` | 14 |
| `get_decl_by_id` | 13 |
| `get_module_by_name` | 3 |
| `get_monomorph` | 2 |
| `get_module_by_id` | 2 |
| `get_type_monomorph` | 2 |
| `get_instance_method_from_instance_id_and_method_name` | 2 |
| `get_function_monomorph` | 1 |
| `get_method_from_typeclass_id_and_name` | 1 |
| `get_instance_method` | 1 |
| `get_callable` | 1 |
| `get_variable` | 1 |
| **total** | 43 |

Totals by file:

| file | count |
|---|---|
| lib/Monomorphize.ml | 8 |
| lib/TypeCheckExpr.ml | 7 |
| lib/CodeGen.ml | 5 |
| lib/Entrypoint.ml | 5 |
| lib/EnvExtras.ml | 4 |
| lib/ExportInstantiation.ml | 3 |
| lib/ExtractionPass.ml | 3 |
| lib/TypingPass.ml | 3 |
| lib/ImportResolution.ml | 2 |
| lib/TypeMatch.ml | 2 |
| lib/TypeParser.ml | 1 |
| **total** | 43 |

Of the 29 `internal_err` calls, 16 share their arm with "found, but of the wrong kind" or have a second `internal_err` for it next to them (the detail column says which). Step 2 of R21 (id lookups stop returning `option`) removes only the "not found" half of those; the other half needs step 3 (the id carries the kind).

Calls where `None` is not an impossible state, although the caller raises `internal_err`:

- `get_type_monomorph` and `get_function_monomorph` (Entrypoint.ml:164, :178, ExportInstantiation.ml:29) take a declaration id and type arguments. "No such monomorph yet" is a normal answer of these functions; `add_or_get_*` in Env depend on it. The three callers rely on monomorphization having run first.
- The three lookups by id + member name fail when the member does not exist. For TypeCheckExpr.ml:436 and Monomorphize.ml:225 a user program gets there (see 2.3).
- TypeMatch.ml:207 looks up a name the user wrote (see 2.3).

### 1.3 Calls of the other lookups outside `lib/Env.ml`

| file:line | function called | key | result and failure |
|---|---|---|---|
| lib/TypeCheckExpr.ml:122 | `module_name_from_id` | `mod_id` stored in a decl | value; raises `internal_err` inside Env (Env.ml:89) |
| lib/TypeCheckExpr.ml:271 | `module_name_from_id` | `mod_id` stored in a decl | same |
| lib/TypingPass.ml:54 | `module_name_from_id` | `mod_id` stored in a decl | same |
| lib/TypingPass.ml:469 | `module_name_from_id` | `mod_id` stored in a decl | same |
| lib/TypeMatch.ml:242 | `visible_instances` | `mod_id` obtained at :222 | list; raises `internal_err` inside Env (Env.ml:358) and uses `Option.get` on `get_decl_by_id` for each imported instance (Env.ml:356) |
| lib/TypingPass.ml:475 | `get_union_cases` | union `decl_id` | list; an unknown id gives `[]` |
| lib/Monomorphize.ml:673 | `get_union_cases` | union `decl_id` | list; each element is re-matched as `UnionCase` (:675-679) |
| lib/ExtractionPass.ml:508 | `module_instances` | `mod_id` of the module being extracted | list |
| lib/ImportResolution.ml:120 | `module_public_instances` | `mod_id` from :117 | list |
| lib/Monomorphize.ml:528 | `get_uninstantiated_monomorphs` | no key | list |
| lib/ExportInstantiation.ml:110 | `get_export_functions` | no key | list |
| lib/Monomorphize.ml:787 | `get_export_functions` | no key | list |
| lib/BodyExtractionPass.ml:17 | `store_function_body` | `decl_id` in a `TFunction` node | update; raises `internal_err` inside Env for unknown id (Env.ml:368), not a function (:384), body already stored (:380) |
| lib/BodyExtractionPass.ml:25 | `store_method_body` | `ins_meth_id` in a `TypedMethodDef` | update; raises `internal_err` inside Env (Env.ml:392, :403) |
| lib/Monomorphize.ml:562 | `store_record_monomorph_definition` | `mono_id` | update; raises `internal_err` inside Env (Env.ml:551 unknown id, :563 wrong kind) |
| lib/Monomorphize.ml:589 | `store_union_monomorph_definition` | `mono_id` | same (Env.ml:551, :575) |
| lib/Monomorphize.ml:619 | `store_function_monomorph_definition` | `mono_id` | same (Env.ml:551, :587) |
| lib/Monomorphize.ml:649 | `store_instance_method_monomorph_definition` | `mono_id` | same (Env.ml:551, :599) |

`add_or_get_record_monomorph`, `add_or_get_union_monomorph`, `add_or_get_function_monomorph` and `add_or_get_instance_method_monomorph` are called at Monomorphize.ml:88, :94, :163, :181, :199 and :780. They look up and register in one step and cannot fail.

Wrappers outside Env that hide a lookup, and their callers: `EnvExtras.get_decl_sident_or_die` (TypeCheckExpr.ml:396, :423), `EnvExtras.get_decl_by_name_or_die` (TypeClasses.ml:260), `EnvExtras.get_decl_name_or_die` (no caller), `CodeGen.get_mono_or_die` (CodeGen.ml:456), `CodeGen.get_decl_name_or_die` (CodeGen.ml:491), `TypeMatch.get_instance` (TypeMatch.ml:210, TypeCheckExpr.ml:420, Monomorphize.ml:207).

### 1.4 Registration functions in `lib/Env.ml`

| function | line | checks a duplicate name? | which |
|---|---|---|---|
| `add_module` | 91 | yes | module name, among all modules: `ensure_no_mod_with_name` (:77) -> `Errors.module_exists` |
| `add_constant` | 140 | yes | `ensure_no_decl_with_name` (:127): any declaration with that name in the same module (constants, records, unions, union cases, functions, typeclasses, typeclass methods) -> `Errors.declaration_already_exists` |
| `add_record` | 153 | yes | same check |
| `add_union` | 166 | yes | same check |
| `add_union_case` | 179 | no | nothing (not against other cases of the union, not against other declarations) |
| `add_function` | 190 | yes | same check as `add_constant` |
| `add_type_class` | 203 | yes | same check |
| `add_type_class_method` | 216 | no | nothing |
| `add_instance` | 227 | no | nothing in Env. The caller checks overlap within the module and the orphan rule first (ExtractionPass.ml:500-513) |
| `add_instance_method` | 238 | no | nothing |
| `add_exported_function` | 612 | no | nothing (two functions may be given the same export name) |
| add_record_monomorph / add_union_monomorph / add_function_monomorph / add_instance_method_monomorph (not exported) | 415 / 422 / 429 / 436 | no | nothing themselves; the exported `add_or_get_*` (:467, :473, :493, :513) first look for a monomorph with the same declaration id and type arguments and return it |

- All calls of the exported `add_*` functions are in `lib/ExtractionPass.ml` (:238, :283, :317, :348, :427, :432, :451, :516, :553, :562, :576).
- The check is one-sided. `ensure_no_decl_with_name` finds union cases and typeclass methods, so a constant, record, union, function or typeclass is rejected when a case or method of that name is already registered. A case or method registered later is not checked against anything.
- `get_decl_by_name` returns the first match in the list (`List.find_opt`, Env.ml:123). Registration puts a declaration at the front, and `store_function_body` moves a function to the front again (Env.ml:372). Which of two unchecked duplicates is found therefore depends on the order of those calls.

## 2. Internal errors (for R24)

91 calls of `internal_err`: 90 in `lib/` and 1 in `cli/`. `rg internal_err` gives 92 lines; the other one is the definition (lib/Error.ml:221). The plan says 93.

Categories:

- **a**: constructor that belongs to another stage.
- **b**: environment lookup by id returned nothing.
- **b+c**: one `| _ ->` arm for both: no declaration for the id, or a declaration of the wrong kind.
- **c**: declaration (or monomorph) of the wrong kind.
- **d**: `body` field in a state that should not occur.
- **e**: loose tree shape.
- **f**: list or option invariant.
- **g**: other.

Notes on how the categories were applied:

- **b+c** is not one of the given categories. 11 calls sit in a single `| _ ->` arm after `Some (Function { ... })` and the like, so the same call reports both a missing declaration and a declaration of the wrong kind.
- **d**: all four are about `body` (stored twice, or not stored yet). Two concern `Function.body` and two concern `InsMethRec.body`. None concerns `external_name`: that field is written at ExtractionPass.ml:423, copied at Env.ml:187-188, :377 and :382, and read nowhere. Every function and method is registered with `body = None` (ExtractionPass.ml:424, :539) and gets its body later from `BodyExtractionPass`, so `None` also means "not typed yet", not only "foreign".
- **e** is read broadly. No `internal_err` guards the two shapes the plan names; those are bare `err` calls (AbstractionPass.ml:22 and :24 for `let` and destructuring outside a block, DesugarPaths.ml:99 for an lvalue that is not a path).
- Sub-groups, written in the table as `b (id + type arguments)`, `g (dead)` and so on. A plain `b` has one id as its key; a plain `c` is a declaration fetched by id.
  - `b (id + type arguments)`: the key is a declaration id and type arguments.
  - `b (id + method name)`: the key is an instance id and a method name.
  - `c (monomorph)`: the thing of the wrong kind is a monomorph fetched by id.
  - `c (from a helper)`: the declaration was returned by `get_instance` or `get_union_cases`, not by an id lookup.
  - `e (concrete tree)`: shape of the `lib/Cst.ml` types.
  - `e (typed tree)`: a type stored on a node of the typed or monomorphic tree.
  - `g (dead)`: `strip_type'` never returns `None`, so the arm cannot run.
  - `g (fixed name)`: environment lookup by a name the compiler fixes itself.
  - `g (resolved name)`: environment lookup by a name that an earlier pass already resolved.
  - `g (user name)`: environment lookup by a name the user wrote and nothing checked.
  - `g (table)`: invariant of the linearity checker's own state table.
  - `g (literal)`: number literal carried as a string.
  - `g (tmp)`: lexical-environment lookup of a compiler-generated temporary.
  - `g (misc)`: one-off.

### 2.1 The rows

| file:line | enclosing function | message | category | note |
|---|---|---|---|---|
| cli/CliUtil.ml:148 | `pos_name` | pos_name called with non-positional argument. | g (misc) | the `arg` list is filtered with `is_pos` and then mapped; a `filter_map` would make the arm unnecessary |
| lib/AbstractionPass.ml:75 | `try_folding' (local parse_bigint)` | Failed to parse '<s>' as a big int. This is a bug in the parser. | g (literal) | integer literal is a `string` in the tree (R18) |
| lib/CodeGen.ml:158 | `gen_type` | Invalid C type | a | `MonoRegionTyVar` reaching C generation |
| lib/CodeGen.ml:170 | `union_type_id` | Union is not a named type? | e (typed tree) | typed tree: the `mono_ty` on `MUnionConstructor` / `MCase` can be any type, the code needs a named type or a reference to one |
| lib/CodeGen.ml:269 | `gen_exp (MArrayIndex)` | Internal error | e (typed tree) | typed tree: the type field of `MArrayIndex` must be a reference type |
| lib/CodeGen.ml:287 | `gen_exp (MSpanIndex)` | Internal error | e (typed tree) | typed tree: same for `MSpanIndex` |
| lib/CodeGen.ml:449 | `get_original_module_name` | Couldn't get module record | b | `get_module_by_id` |
| lib/CodeGen.ml:451 | `get_original_module_name` | Couldn't get function | b+c | `get_decl_by_id`, expects `Function` |
| lib/CodeGen.ml:453 | `get_original_module_name` | Couldn't get monomorph | b+c | `get_monomorph`, expects `MonoFunction` |
| lib/CodeGen.ml:472 | `get_mono_or_die` | Couldn't get monomorph | b | `get_monomorph` |
| lib/CodeGen.ml:481 | `get_decl_name_or_die` | decl has no name | c | the decl is an `Instance`, the only kind without a name |
| lib/CodeGen.ml:483 | `get_decl_name_or_die` | Couldn't find decl | b | `get_decl_by_id` |
| lib/CombiningPass.ml:404 | `parse_decl` | Couldn't parse declaration declaration | e (concrete tree) | concrete tree: `concrete_decl_name` is `None` only for `ConcreteInstanceDecl`; the second match cannot know that |
| lib/CombiningPass.ml:429 | `parse_def` | Couldn't parse definition | e (concrete tree) | concrete tree: same for `def_name` and `ConcreteInstanceDef` |
| lib/Entrypoint.ml:168 | `get_root_capability_monomorph` | No monomorph of RootCapability. | b (id + type arguments) | `get_type_monomorph` by declaration id + empty bindings |
| lib/Entrypoint.ml:170 | `get_root_capability_monomorph` | Can't find the RootCapability type in the environment. | g (fixed name) | lookup by the fixed name `Austral.Pervasive.RootCapability`; the arm covers no decl and not-a-`Record` |
| lib/Entrypoint.ml:182 | `get_exit_code_monomorph` | No monomorph of ExitCode. | b (id + type arguments) | `get_type_monomorph` |
| lib/Entrypoint.ml:184 | `get_exit_code_monomorph` | Can't find the ExitCode type in the environment. | g (fixed name) | lookup by the fixed name `Austral.Pervasive.ExitCode`; the arm covers no decl and not-a-`Union` |
| lib/Env.ml:89 | `module_name_from_id` | No module with this ID | b |  |
| lib/Env.ml:358 | `visible_instances` | No module with ID. | b |  |
| lib/Env.ml:368 | `store_function_body` | No function with this ID: <id> | b |  |
| lib/Env.ml:380 | `replace_function_body` | function `<name>` already has a body | d | `Function.body` is already `Some` |
| lib/Env.ml:384 | `replace_function_body` | not a function. | c |  |
| lib/Env.ml:392 | `store_method_body` | No instance method with this ID <id> | b |  |
| lib/Env.ml:403 | `replace_method_body` | function `<name>` already has a body | d | `InsMethRec.body` is already `Some` |
| lib/Env.ml:551 | `pop_monomorph` | monomorph with this ID does not exist: <id> | b |  |
| lib/Env.ml:563 | `store_record_monomorph_definition` | Couldn't find monomorph record definition with ID: <id> | c (monomorph) | the monomorph exists but is of another kind |
| lib/Env.ml:575 | `store_union_monomorph_definition` | Couldn't find monomorph union definition with ID: <id> | c (monomorph) | same |
| lib/Env.ml:587 | `store_function_monomorph_definition` | Couldn't find monomorph function definition with ID: <id> | c (monomorph) | same |
| lib/Env.ml:599 | `store_instance_method_monomorph_definition` | Couldn't find monomorph instance method definition with ID: <id> | c (monomorph) | same |
| lib/EnvExtras.ml:17 | `get_module_or_die` | No module with ID. | b |  |
| lib/EnvExtras.ml:27 | `get_decl_by_name_or_die` | No declaration with name `<name>`. | g (resolved name) | its one caller passes the name inside a resolved `NamedType` (TypeClasses.ml:260) |
| lib/EnvExtras.ml:40 | `get_decl_sident_or_die` | decl with ID `<id>` doesn't exist | b |  |
| lib/EnvExtras.ml:53 | `get_decl_name_or_die` | decl with ID `<id>` doesn't exist | b | the function has no caller in `lib/` or `cli/` |
| lib/EnvUtils.ml:71 | `union_case_to_typed_case` | Declaration is not a union case | c (from a helper) | applied to the result of `get_union_cases`, which already filtered on `UnionCase` |
| lib/ExportInstantiation.ml:33 | `get_export_monomorph` | No monomorph of exported function `<name>` | b (id + type arguments) | `get_function_monomorph` by declaration id + empty bindings |
| lib/ExportInstantiation.ml:35 | `get_export_monomorph` | Can't find the exported fuction in the environment. | b+c | expects `Function` |
| lib/ExportInstantiation.ml:42 | `get_export_data` | Can't find the exported fuction in the environment. | b+c | expects `Function` |
| lib/ExtractionPass.ml:223 | `extract` | Couldn't find module `<name>`. | g (resolved name) | module looked up by a name read back from the resolved import maps |
| lib/LinearityCheck.ml:55 | `get_entry_or_fail` | variable `<name>` not in state table. Table contents: ... | g (table) | lookup by variable name |
| lib/LinearityCheck.ml:68 | `add_entry` | An entry exists in the state table with this name: <name> | g (table) | duplicate; the comment says an earlier check should have caught it |
| lib/LinearityCheck.ml:87 | `update_tbl` | Tried to update the state of the variable `<name>`, but no such variable exists in the state table. ... | g (table) |  |
| lib/LinearityCheck.ml:101 | `remove_entry` | Tried to update the state of the variable `<name>`, but no such variable exists in the state table. ... | g (table) |  |
| lib/LinearityCheck.ml:143 | `mark_pending` | Identifier twice marked pending. | g (table) |  |
| lib/LinearityCheck.ml:149 | `remove_pending` | Tried to remove_pending an identifier not on the list of pending variables. | g (table) |  |
| lib/LinearityCheck.ml:301 | `partition` | Impossible | g (misc) | a negative count; the argument is an `int` |
| lib/LinearityCheck.ml:325 | `tables_are_consistent` | bad state table | g (table) | second lookup of a name just taken from the same table (close to f) |
| lib/LinearityCheck.ml:332 | `tables_are_consistent` | bad state table | g (table) | same |
| lib/Monomorphize.ml:97 | `monomorphize_ty` | named type `<name>`points to something that isn't a type | g (resolved name) | declaration of the wrong kind, found by the name inside `SNamedType` |
| lib/Monomorphize.ml:99 | `monomorphize_ty` | Couldn't get declaration for named type `<name>`. | g (resolved name) | nothing found by the name inside `SNamedType` |
| lib/Monomorphize.ml:217 | `monomorphize_expr (TVarMethodCall)` | Couldn't find instance for var method call `<m>` with typeclass ID `<id>`. | c (from a helper) | the decl returned by `TypeMatch.get_instance` is not an `Instance` |
| lib/Monomorphize.ml:227 | `monomorphize_expr (TVarMethodCall)` | Couldn't find instance method from instance ID `<id>` and method name `<m>`. | b (id + method name) | key is instance id + method name. Reachable from a user program (2.3) |
| lib/Monomorphize.ml:663 | `get_record_definition` | couldn't get record definition for ID `<id>`. | b+c | expects `Record` |
| lib/Monomorphize.ml:670 | `get_union_typarams` | couldn't get union typeparams for ID `<id>`. | b+c | expects `Union` |
| lib/Monomorphize.ml:679 | `get_union_typed_cases (local mapper)` | ID `<id>` is not a union | c (from a helper) | element of the `get_union_cases` result is not a `UnionCase` |
| lib/Monomorphize.ml:690 | `get_function_definition` | function `<name>` has no body | d | `Function.body` is `None` |
| lib/Monomorphize.ml:692 | `get_function_definition` | ID `<id>is not a function | b+c | expects `Function` |
| lib/Monomorphize.ml:703 | `get_method_definition` | method with ID `<id>` has no body | d | `InsMethRec.body` is `None` |
| lib/Monomorphize.ml:705 | `get_method_definition` | ID `<id>`not an instance | b+c | expects `Instance` |
| lib/Monomorphize.ml:707 | `get_method_definition` | ID `<id>`not an instance method | b | `get_instance_method` |
| lib/Monomorphize.ml:783 | `monomorphize_wrapper` | Didn't find wrapper in env. | b+c | expects `Function` |
| lib/MtastUtil.ml:70 | `get_type (MDeref)` | a dereference expression was constructed whose argument is not a reference type. | e (typed tree) | typed tree: `MDeref` of a non-reference |
| lib/TastUtil.ml:110 | `get_type (TDeref)` | a dereference expression was constructed whose argument is not a reference type. | e (typed tree) | typed tree: `TDeref` of a non-reference |
| lib/TypeCheckExpr.ml:189 | `check_argument_list'` | couldn't check argument list in module `<m>` with bindings: ... | f | the two lists differ in length; `check_arity` ran just before |
| lib/TypeCheckExpr.ml:273 | `augment_callable` | no type name found for ID `<id>` | b+c | expects `Union` |
| lib/TypeCheckExpr.ml:281 | `augment_callable` | no type parameter found for ID `<id>` | b+c | expects `TypeClass` |
| lib/TypeCheckExpr.ml:432 | `augment_method_call` | Couldn't get instance while getting type parameters | c (from a helper) | the decl returned by `get_instance` is not an `Instance` |
| lib/TypeCheckExpr.ml:438 | `augment_method_call` | Couldn't get instance method for `<m>` | b (id + method name) | key is instance id + the method name the user wrote. Reachable from a user program (2.3) |
| lib/TypeCheckExpr.ml:445 | `augment_method_call` | couldn't extract dispatch type. | f | `get_binding` gave `None` for the typeclass parameter although `check_bindings` passed |
| lib/TypeCheckExpr.ml:589 | `augment_temporary` | Temporary variable does not exist in lexenv: <name> | g (tmp) | depends on the order in which `LiftControlPass` emits `LetTmp` |
| lib/TypeMatch.ml:216 | `try_constraint` | Type parameter constraint refers to a declaration which is not a typeclass. | g (user name) | wrong kind, found by the name the user wrote in the constraint. Reachable from a user program (2.3) |
| lib/TypeMatch.ml:218 | `try_constraint` | No typeclass with the given name. This should have been validated in the extraction pass. | g (user name) | nothing found by the name the user wrote in the constraint. Reachable from a user program (2.3) |
| lib/TypeMatch.ml:248 | `get_instance` | Multiple instances satisfy this call. The compiler should validate that there are no overlapping instances. | g (misc) | relies on a check made elsewhere (overlap is only checked inside one module, ExtractionPass.ml:500-510) |
| lib/TypeMatch.ml:265 | `parse_bigint` | Failed to parse '<s>' as a big int. This is a bug in the parser. | g (literal) | integer literal is a `string` in the tree (R18) |
| lib/TypeParser.ml:184 | `effective_universe` | effective_universe called with a region type | g (misc) | a user-defined type whose declared universe is `Region`; ExtractionPass.ml:312/:342 reject that, but the slots are parsed first (:289). Reachable from a user program (2.3) |
| lib/TypeStripping.ml:37 | `strip_type` | strip_type called with a region type as its argument | g (dead) |  |
| lib/TypeStripping.ml:62 | `strip_type' (ReadRef)` | unable to strip read ref type | g (dead) |  |
| lib/TypeStripping.ml:64 | `strip_type' (ReadRef)` | read ref instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:72 | `strip_type' (WriteRef)` | unable to strip write ref type | g (dead) |  |
| lib/TypeStripping.ml:74 | `strip_type' (WriteRef)` | write ref instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:82 | `strip_type' (Span)` | unable to strip span type | g (dead) |  |
| lib/TypeStripping.ml:84 | `strip_type' (Span)` | span instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:92 | `strip_type' (SpanMut)` | unable to strip span! type | g (dead) |  |
| lib/TypeStripping.ml:94 | `strip_type' (SpanMut)` | span! instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:110 | `strip_type' (Address)` | Address type instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:116 | `strip_type' (Pointer)` | Pointer type instantiated with a region type. | g (dead) |  |
| lib/TypeStripping.ml:121 | `strip_type' (FnPtr)` | Function pointer type instantiated with a region type. | g (dead) |  |
| lib/TypeSystem.ml:28 | `type_universe` | You shouldn't be asking for the type_universe of a MonoTy | a | `MonoTy` inside `Type.ty` before monomorphization |
| lib/TypingPass.ml:145 | `augment_stmt (AssignTmp)` | Unknown temporary. | g (tmp) | `AssignTmp` for a temporary that no `LetTmp` declared |
| lib/Util.ml:146 | `last` | Util.last called with an empty list. | f | empty list |
| lib/Util.ml:154 | `butlast` | Util.butlast called with an empty list. | f | empty list |

### 2.2 Totals

By category:

| category | count |
|---|---|
| a | 2 |
| b | 17 |
| b+c | 11 |
| c | 10 |
| d | 4 |
| e | 7 |
| f | 4 |
| g | 36 |
| **total** | 91 |

With sub-groups:

| category | count |
|---|---|
| a | 2 |
| b | 12 |
| b (id + type arguments) | 3 |
| b (id + method name) | 2 |
| b+c | 11 |
| c | 2 |
| c (monomorph) | 4 |
| c (from a helper) | 4 |
| d | 4 |
| e (concrete tree) | 2 |
| e (typed tree) | 5 |
| f | 4 |
| g (dead) | 12 |
| g (fixed name) | 2 |
| g (resolved name) | 4 |
| g (user name) | 2 |
| g (table) | 8 |
| g (literal) | 2 |
| g (tmp) | 2 |
| g (misc) | 4 |
| **total** | 91 |

Totals by file:

| file | count |
|---|---|
| lib/Monomorphize.ml | 13 |
| lib/Env.ml | 12 |
| lib/TypeStripping.ml | 12 |
| lib/CodeGen.ml | 10 |
| lib/LinearityCheck.ml | 9 |
| lib/TypeCheckExpr.ml | 7 |
| lib/Entrypoint.ml | 4 |
| lib/EnvExtras.ml | 4 |
| lib/TypeMatch.ml | 4 |
| lib/ExportInstantiation.ml | 3 |
| lib/CombiningPass.ml | 2 |
| lib/Util.ml | 2 |
| cli/CliUtil.ml | 1 |
| lib/AbstractionPass.ml | 1 |
| lib/EnvUtils.ml | 1 |
| lib/ExtractionPass.ml | 1 |
| lib/MtastUtil.ml | 1 |
| lib/TastUtil.ml | 1 |
| lib/TypeParser.ml | 1 |
| lib/TypeSystem.ml | 1 |
| lib/TypingPass.ml | 1 |
| **total** | 91 |

Category by file:

| file | a | b | b+c | c | d | e | f | g | total |
|---|---|---|---|---|---|---|---|---|---|
| lib/Monomorphize.ml |  | 2 | 5 | 2 | 2 |  |  | 2 | 13 |
| lib/Env.ml |  | 5 |  | 5 | 2 |  |  |  | 12 |
| lib/TypeStripping.ml |  |  |  |  |  |  |  | 12 | 12 |
| lib/CodeGen.ml | 1 | 3 | 2 | 1 |  | 3 |  |  | 10 |
| lib/LinearityCheck.ml |  |  |  |  |  |  |  | 9 | 9 |
| lib/TypeCheckExpr.ml |  | 1 | 2 | 1 |  |  | 2 | 1 | 7 |
| lib/Entrypoint.ml |  | 2 |  |  |  |  |  | 2 | 4 |
| lib/EnvExtras.ml |  | 3 |  |  |  |  |  | 1 | 4 |
| lib/TypeMatch.ml |  |  |  |  |  |  |  | 4 | 4 |
| lib/ExportInstantiation.ml |  | 1 | 2 |  |  |  |  |  | 3 |
| lib/CombiningPass.ml |  |  |  |  |  | 2 |  |  | 2 |
| lib/Util.ml |  |  |  |  |  |  | 2 |  | 2 |
| cli/CliUtil.ml |  |  |  |  |  |  |  | 1 | 1 |
| lib/AbstractionPass.ml |  |  |  |  |  |  |  | 1 | 1 |
| lib/EnvUtils.ml |  |  |  | 1 |  |  |  |  | 1 |
| lib/ExtractionPass.ml |  |  |  |  |  |  |  | 1 | 1 |
| lib/MtastUtil.ml |  |  |  |  |  | 1 |  |  | 1 |
| lib/TastUtil.ml |  |  |  |  |  | 1 |  |  | 1 |
| lib/TypeParser.ml |  |  |  |  |  |  |  | 1 | 1 |
| lib/TypeSystem.ml | 1 |  |  |  |  |  |  |  | 1 |
| lib/TypingPass.ml |  |  |  |  |  |  |  | 1 | 1 |

### 2.3 Calls that a user program reaches

Five of the 91 were reached with small programs, using the binary already built in `_build_coverage/` of the copy (run from a scratch directory, `--target-type=tc`). The programs are in `inventory-probe/` next to this file. Each run printed `Internal compiler error:` followed by the message of the row, and ended with exit code 255.

| file:line | category | program | what the program does |
|---|---|---|---|
| lib/TypeCheckExpr.ml:438 | b (id + method name) | MissingMethod.aum | an instance leaves out a method of its typeclass; the method is called on that type |
| lib/Monomorphize.ml:227 | b (id + method name) | MissingMethodGeneric.aum | the same, called through a generic function with a constraint |
| lib/TypeMatch.ml:216 | g (user name) | ConstraintRecord.aum | a type parameter constraint names a record; the generic function is called |
| lib/TypeMatch.ml:218 | g (user name) | ConstraintMissing.aum | a type parameter constraint names nothing that exists; the generic function is called |
| lib/TypeParser.ml:184 | g (misc) | RegionRecord.aum | a record declared in the `Region` universe has a slot of its own type |

`docs/plan.md` (line 326) already knows the first case, and issue #128 lists internal errors of this kind; whether all five are in it was not checked. No file under `test-programs/` expects the text `Internal compiler error`.

TypeMatch.ml:248 ("Multiple instances satisfy this call") looks reachable too, because overlap is only checked inside one module; no program was found for it.

### 2.4 What each planned item can remove

| category | calls | what would remove it | status | note |
|---|---|---|---|---|
| b | 12 | R21 step 2: id lookups stop returning `option` | the plan says so | 5 of the 12 are inside `lib/Env.ml` already; the plan keeps one such check there |
| b+c | 11 | R21 steps 2 and 3 | the plan says so | step 2 alone turns each of them into a c |
| c | 2 | R21 step 3: the id carries the kind | the plan says so | CodeGen.ml:481, Env.ml:384 |
| c (monomorph) | 4 | R21 step 3, if `mono_id` gets a kind as well as `decl_id` | depends on a choice the plan does not state |  |
| c (from a helper) | 4 | R21, if `get_instance` and `get_union_cases` return something narrower than `decl` | depends on a choice the plan does not state |  |
| b (id + type arguments) | 3 | nothing in R21: "no such monomorph" is a normal answer of the function | depends on a choice the plan does not state | the callers rely on monomorphization having run |
| b (id + method name) | 2 | nothing in R21: the method can really be missing | depends on a choice the plan does not state | a user program reaches both (2.3); a named error or an earlier check changes what such a program prints |
| g (fixed name) | 2 | nothing in R21 or R17: the lookup by name stays | depends on a choice the plan does not state |  |
| g (resolved name) | 4 | R21, if a named type and an import map carry an id instead of a name | depends on a choice the plan does not state |  |
| g (user name) | 2 | nothing in R21: the name was never checked | depends on a choice the plan does not state | a user program reaches both (2.3) |
| a | 2 | R24: `MonoTy` and region type variables | the plan says so |  |
| g (dead) | 12 | R24: `lib/TypeStripping.ml` | the plan says so |  |
| d | 4 | R24: function declaration as a sum, if the body also stops being filled in later | depends on a choice the plan does not state |  |
| e (concrete tree) | 2 | R24: syntax tree; the plan names other shapes (lvalue, `let`) | depends on a choice the plan does not state |  |
| e (typed tree) | 5 | no item names it | depends on a choice the plan does not state |  |
| g (literal) | 2 | R18 | the plan says so |  |
| f, g (table), g (tmp), g (misc) | 18 | no item | outside the plan |  |
| **total** | 91 |  |  |  |

| status | count |
|---|---|
| the plan says so | 41 |
| depends on a choice the plan does not state | 32 |
| outside the plan | 18 |
| **total** | 91 |

The plan wants 20 or fewer. 18 calls are outside the plan, and the plan keeps one id check inside `lib/Env.ml`. That leaves room for 1 more: of the 32 calls that depend on an unstated choice, all but 1 have to go.

Guards on the same stage-crossing constructors that are not `internal_err` and so are not in the table: TypeMatch.ml:159 (`err "match_type called with MonoTy argument"`), TypeSystem.ml:46 (`err "is_numeric called with MonoTy argument"`), TypeClasses.ml:157 and :223 (`err "Not allowed"` on `MonoTy`), TypeStripping.ml:104 (`err` on a type variable left over), CodeGen.ml:580 (`err "Not allowed"` on `MonoRegionTyVar`), ExportInstantiation.ml:91 (`Errors.disallowed_type` on `MonoTy`).

## 3. Catch-all match arms on declaration, expression and statement types (for R25)

### 3.1 How the search was done

1. `rg` for arms that are a lone wildcard or a lone variable at the start of a line: `| _ ->`, `| x ->`, `| Some _ ->`, `| (Some _) ->`, `| _::_ ->`, and tuples of wildcards. That gives 206 `| _ ->` arms and 33 arms of the other forms in `lib/*.ml` and `cli/*.ml`.
2. Separate searches for forms the first one misses: arms with a `when` guard or an `as` (none), a wildcard inside a tuple pattern (none on these types), a `match` written on one line (one, on `Type.ty`), a pattern that continues on the next line (none).
3. For each hit a script printed the head of the `match` or `function` and the patterns of the sibling arms. The type was read from the constructors, the annotations and the `open` lines. Hits on other types (`Type.ty`, `MonoType.mono_ty`, lists, options, strings, `CRepr`, `Common.pragma`, the command-line `arg`) were dropped.
4. A match on `decl option` is listed when an arm names a constructor of `decl` under `Some` and a later arm (`| _ ->` or `| Some _ ->`) takes the rest.
5. Cross-check: `tools/refactoring/expected/fragile.tsv` holds the compiler's warning-4 output recorded at R7 (e2afef8). `lib/` has changed since, so it is evidence, not proof. Per file and per type, half of each recorded number equals the number of arms found here, for every type in scope.

What this can miss: a catch-all hidden in an or-pattern on a single line with other constructors, and matches in `lib/Parser.mly` and `lib/Lexer.mll`, which were not searched.

### 3.2 The rows

78 catch-all arms. "Named in the other arms" lists the constructors of the matched type; `Some (X)` means the match is on an `option` of the type.

**EnvTypes.decl** (37 arms)

| file:line | type matched | enclosing function | named in the other arms | the catch-all arm | detail |
|---|---|---|---|---|---|
| lib/Env.ml:260 | EnvTypes.decl | `get_method_from_typeclass_id_and_name (pred)` | TypeClassMethod | default value | `false` (filter over all declarations) |
| lib/Env.ml:270 | EnvTypes.decl | `module_instances (pred)` | Instance | default value | `false` (filter over all declarations) |
| lib/Env.ml:281 | EnvTypes.decl | `module_public_instances (pred)` | Instance | default value | `false` (filter) |
| lib/Env.ml:290 | EnvTypes.decl | `get_union_cases (pred)` | UnionCase | default value | `false` (filter) |
| lib/Env.ml:305 | EnvTypes.decl option | `get_callable` | Some (Union) | bare err | `err "No union with ID"`; also covers `None` (result of `get_decl_by_id`) |
| lib/Env.ml:324 | EnvTypes.decl | `get_callable` | Record, UnionCase, Function, TypeClassMethod | default value | `None`: Constant, Union, TypeClass, Instance are not callable (decl from a name lookup) |
| lib/Env.ml:339 | EnvTypes.decl | `get_variable` | Constant | default value | `None` (decl from a name lookup) |
| lib/Env.ml:350 | EnvTypes.decl | `visible_instances (pred)` | Instance | default value | `false` (filter) |
| lib/Env.ml:383 | EnvTypes.decl | `replace_function_body` | Function | internal_err | decl was fetched by id at :364 |
| lib/EnvUtils.ml:70 | EnvTypes.decl | `union_case_to_typed_case` | UnionCase | internal_err | applied to `get_union_cases` results |
| lib/ExportInstantiation.ml:34 | EnvTypes.decl option | `get_export_monomorph` | Some (Function) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/ExportInstantiation.ml:41 | EnvTypes.decl option | `get_export_data` | Some (Function) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/Entrypoint.ml:107 | EnvTypes.decl | `check_entrypoint_validity` | Function | named user error | `Errors.not_function` (decl from a name lookup) |
| lib/Entrypoint.ml:169 | EnvTypes.decl option | `get_root_capability_monomorph` | Some (Record) | internal_err | also covers `None` (`get_decl_by_name`, fixed name) |
| lib/Entrypoint.ml:183 | EnvTypes.decl option | `get_exit_code_monomorph` | Some (Union) | internal_err | also covers `None` (`get_decl_by_name`, fixed name) |
| lib/Monomorphize.ml:96 | EnvTypes.decl | `monomorphize_ty` | Record, Union | internal_err | decl from a name lookup (`SNamedType`) |
| lib/Monomorphize.ml:217 | EnvTypes.decl | `monomorphize_expr` | Instance | internal_err | decl returned by `get_instance` |
| lib/Monomorphize.ml:662 | EnvTypes.decl option | `get_record_definition` | Some (Record) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/Monomorphize.ml:669 | EnvTypes.decl option | `get_union_typarams` | Some (Union) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/Monomorphize.ml:678 | EnvTypes.decl | `get_union_typed_cases (mapper)` | UnionCase | internal_err | applied to `get_union_cases` results |
| lib/Monomorphize.ml:691 | EnvTypes.decl option | `get_function_definition` | Some (Function) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/Monomorphize.ml:704 | EnvTypes.decl option | `get_method_definition` | Some (Instance) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/Monomorphize.ml:782 | EnvTypes.decl option | `monomorphize_wrapper` | Some (Function) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/CodeGen.ml:450 | EnvTypes.decl option | `get_original_module_name` | Some (Function) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/ExtractionPass.ml:488 | EnvTypes.decl | `extract (CInstance)` | TypeClass | named user error | `Errors.instance_for_non_typeclass` (decl from a name lookup) |
| lib/ExtractionPass.ml:507 | EnvTypes.decl | `extract (CInstance)` | Instance | default value | `false` (filter over `module_instances`, which already holds only instances) |
| lib/ExtractionPass.ml:526 | EnvTypes.decl option | `extract (method_map)` | Some (TypeClassMethod) | named user error | `Errors.typeclass_no_method`; also covers `None` (`get_method_from_typeclass_id_and_name`, which only returns methods) |
| lib/TypeCheckExpr.ml:124 | EnvTypes.decl option | `get_record_definition` | Some (Record) | named user error | the arm is `\| Some _ ->`: `Errors.path_not_record` (`get_decl_by_name`) |
| lib/TypeCheckExpr.ml:272 | EnvTypes.decl option | `augment_callable` | Some (Union) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/TypeCheckExpr.ml:280 | EnvTypes.decl option | `augment_callable` | Some (TypeClass) | internal_err | also covers `None` (`get_decl_by_id`) |
| lib/TypeCheckExpr.ml:432 | EnvTypes.decl | `augment_method_call` | Instance | internal_err | decl returned by `get_instance` |
| lib/TypeCheckExpr.ml:569 | EnvTypes.decl option | `augment_variable` | Some (Function) | generic handling | anything else, and `None`, goes on to `get_variable` (`get_decl_by_name`) |
| lib/TypingPass.ml:56 | EnvTypes.decl option | `get_record_definition` | Some (Record) | named user error | the arm is `\| Some _ ->`: `Errors.path_not_record` (`get_decl_by_name`) |
| lib/TypingPass.ml:479 | EnvTypes.decl option | `get_union_type_definition` | Some (Union) | named user error | `Errors.case_non_union`; also covers `None` (`get_decl_by_name`) |
| lib/TypeMatch.ml:215 | EnvTypes.decl option | `try_constraint` | Some (TypeClass) | internal_err | the arm is `\| Some _ ->` (`get_decl_by_name`) |
| lib/TypeMatch.ml:238 | EnvTypes.decl | `get_instance (pred)` | Instance | default value | `None` (filter over `visible_instances`, which already holds only instances) |
| lib/TypeClasses.ml:230 | EnvTypes.decl | `check_instance_locally_unique` | Instance | default value | `false` (filter over a list of instances) |

**EnvTypes.monomorph** (8 arms)

| file:line | type matched | enclosing function | named in the other arms | the catch-all arm | detail |
|---|---|---|---|---|---|
| lib/Env.ml:460 | EnvTypes.monomorph | `get_type_monomorph (pred)` | MonoRecordDefinition, MonoUnionDefinition | default value | `false` (filter over all monomorphs) |
| lib/Env.ml:486 | EnvTypes.monomorph | `get_function_monomorph (pred)` | MonoFunction | default value | `false` (filter) |
| lib/Env.ml:506 | EnvTypes.monomorph | `get_instance_method_monomorph (pred)` | MonoInstanceMethod | default value | `false` (filter) |
| lib/Env.ml:562 | EnvTypes.monomorph | `store_record_monomorph_definition` | MonoRecordDefinition | internal_err | monomorph was fetched by id |
| lib/Env.ml:574 | EnvTypes.monomorph | `store_union_monomorph_definition` | MonoUnionDefinition | internal_err | same |
| lib/Env.ml:586 | EnvTypes.monomorph | `store_function_monomorph_definition` | MonoFunction | internal_err | same |
| lib/Env.ml:598 | EnvTypes.monomorph | `store_instance_method_monomorph_definition` | MonoInstanceMethod | internal_err | same |
| lib/CodeGen.ml:452 | EnvTypes.monomorph option | `get_original_module_name` | Some (MonoFunction) | internal_err | also covers `None` (`get_monomorph`) |

**Types of `lib/Cst.ml`** (14 arms)

| file:line | type matched | enclosing function | named in the other arms | the catch-all arm | detail |
|---|---|---|---|---|---|
| lib/CombiningPass.ml:163 | Cst.concrete_def | `match_decls (ConcreteConstantDecl)` | ConcreteConstantDef | named user error | `Errors.declaration_kind_mismatch ~expected:"constant"` |
| lib/CombiningPass.ml:199 | Cst.concrete_def (the patterns also open `concrete_record`, `concrete_union`) | `match_decls (ConcreteOpaqueTypeDecl)` | ConcreteRecordDef (ConcreteRecord), ConcreteUnionDef (ConcreteUnion) | named user error | `Errors.declaration_kind_mismatch ~expected:"type"` |
| lib/CombiningPass.ml:231 | Cst.concrete_def | `match_decls (ConcreteFunctionDecl)` | ConcreteFunctionDef | named user error | `Errors.declaration_kind_mismatch ~expected:"function"` |
| lib/CombiningPass.ml:254 | Cst.concrete_def (also opens `concrete_instance`) | `match_decls (ConcreteInstanceDecl)` | ConcreteInstanceDef (ConcreteInstance) | named user error | `Errors.declaration_kind_mismatch ~expected:"instance"` |
| lib/CombiningPass.ml:256 | Cst.concrete_decl | `match_decls` | ConcreteConstantDecl, ConcreteOpaqueTypeDecl, ConcreteFunctionDecl, ConcreteInstanceDecl | bare err | `err "Invalid decl in this context"` for ConcreteRecordDecl, ConcreteUnionDecl, ConcreteTypeClassDecl (the caller handles those three itself) |
| lib/CombiningPass.ml:387 | Cst.concrete_decl (also opens `concrete_record`, `concrete_union`, `concrete_typeclass`) | `parse_decl` | ConcreteRecordDecl (ConcreteRecord), ConcreteUnionDecl (ConcreteUnion), ConcreteTypeClassDecl (ConcreteTypeClass) | generic handling | every other declaration: find the body definition and call `match_decls` (`Errors.missing_body_definition` if there is none) |
| lib/CombiningPass.ml:403 | Cst.concrete_decl | `parse_decl (declaration without a name)` | ConcreteInstanceDecl | internal_err |  |
| lib/CombiningPass.ml:428 | Cst.concrete_def (also opens `concrete_instance`) | `parse_def (definition without a name)` | ConcreteInstanceDef (ConcreteInstance) | internal_err |  |
| lib/CstUtil.ml:85 | Cst.concrete_decl | `has_instance_decl (pred)` | ConcreteInstanceDecl | default value | `false` |
| lib/CstUtil.ml:93 | Cst.concrete_def | `get_instance_def (filter)` | ConcreteInstanceDef | default value | `None` |
| lib/CstUtil.ml:113 | Cst.concrete_arglist (the pattern also opens `cexpr`) | `make_pragma (Foreign_Import)` | ConcreteNamedArgs [(a, CStringConstant _)] | named user error | `Errors.pragma_argument_error`; the arm also covers named argument lists of another length |
| lib/CstUtil.ml:128 | Cst.concrete_arglist (also opens `cexpr`) | `make_pragma (Foreign_Export)` | ConcreteNamedArgs [(a, CStringConstant _)] | named user error | same |
| lib/CstUtil.ml:140 | Cst.concrete_arglist | `make_pragma (Unsafe_Module)` | ConcretePositionalArgs [] | named user error | `Errors.pragma_argument_error`; also covers a non-empty positional list |
| lib/AbstractionPass.ml:181 | Cst.cstmt | `let_reshape` | CLet, CDestructure | generic handling | the arm is `\| s ->`: every other statement goes to `abs_stmt` |

**Stage types of `lib/Stages.ml`** (19 arms)

| file:line | type matched | enclosing function | named in the other arms | the catch-all arm | detail |
|---|---|---|---|---|---|
| lib/AbstractionPass.ml:39 | Stages.Ast.astmt | `abs_stmt (CIf)` | ASkip | generic handling | builds `AIf`; an `ASkip` else-branch builds `AWhen` |
| lib/AbstractionPass.ml:91 | Stages.Ast.aexpr | `try_folding'` | IntConstant, ArithmeticExpression | default value | `None` (not foldable) |
| lib/ReturnCheck.ml:61 | Stages.Ast.aexpr | `ends_in_return (ADiscarding)` | FunctionCall | default value | `false` |
| lib/DesugarPaths.ml:98 | Stages.AstLC.aexpr | `transform_lvalue` | Path | bare err | `err "Invalid lvalue."` |
| lib/DesugarPaths.ml:113 | Stages.AstLC.aexpr | `transform_stmt (AAssign)` | Path (name, []) | generic handling | general assignment through `transform_lvalue`; also covers `Path` with a non-empty element list |
| lib/TypeCheckExpr.ml:102 | Stages.AstDB.aexpr | `is_int_constant` | IntConstant | default value | `false` |
| lib/TypeCheckExpr.ml:108 | Stages.AstDB.aexpr | `is_float_constant` | FloatConstant | default value | `false` |
| lib/TypeCheckExpr.ml:830 | Stages.Tast.texpr | `augment_typecast (local is_int_expr)` | TIntConstant | default value | `false` |
| lib/TypeCheckExpr.ml:833 | Stages.Tast.texpr | `augment_typecast (local is_float_expr)` | TFloatConstant | default value | `false` |
| lib/TypingPass.ml:68 | Stages.Tast.texpr | `is_compatible_with_index_type` | TIntConstant | generic handling | the arm is `\| e ->`: compares `get_type e` with the index type |
| lib/TypingPass.ml:94 | Stages.AstDB.astmt (the patterns also open `aexpr`: `Temporary`) | `lifted_assignment_span` | AAssign (_, _, Temporary _), AAssignVar (_, _, Temporary _), ABorrow | default value | `None`; also covers assignments whose right-hand side is not a temporary |
| lib/TypingPass.ml:105 | Stages.AstDB.aexpr | `declare_assigned_temporary` | Temporary | default value | `()` |
| lib/TypeMatch.ml:309 | Stages.Tast.texpr | `match_type_with_value (Integer)` | TIntConstant, TCast (TIntConstant, _) | generic handling | `match_type` on the type of the expression |
| lib/TypeMatch.ml:317 | Stages.Tast.texpr | `match_type_with_value (SingleFloat)` | TFloatConstant, TCast (TFloatConstant, _) | generic handling | same |
| lib/TypeMatch.ml:325 | Stages.Tast.texpr | `match_type_with_value (DoubleFloat)` | TFloatConstant, TCast (TFloatConstant, _) | generic handling | same |
| lib/BodyExtractionPass.ml:20 | Stages.Tast.typed_decl | `extract_decl` | TFunction, TInstance | default value | returns `env` unchanged |
| lib/LinearityCheck.ml:768 | Stages.Tast.typed_decl | `check_decl_linearity` | TFunction, TInstance | default value | `()` |
| lib/CodeGen.ml:625 | Stages.Mtast.mdecl | `gen_type_decl` | (none: `\| _ ->` is the only arm; two arms are commented out) | default value | `None`: the function returns `None` for every declaration. The compiler does not count this one as fragile |
| lib/CodeGen.ml:649 | Stages.Mtast.mdecl | `gen_fun_decl` | MFunction, MFunctionMonomorph, MForeignFunction, MConcreteInstance, MMethodMonomorph | default value | `None` for MConstant, MRecord, MRecordMonomorph, MUnion, MUnionMonomorph |

Totals by type:

| type | count |
|---|---|
| EnvTypes.decl | 37 |
| EnvTypes.monomorph | 8 |
| Cst.concrete_def | 6 |
| Stages.Tast.texpr | 6 |
| Cst.concrete_decl | 4 |
| Cst.concrete_arglist | 3 |
| Stages.AstDB.aexpr | 3 |
| Stages.Ast.aexpr | 2 |
| Stages.AstLC.aexpr | 2 |
| Stages.Tast.typed_decl | 2 |
| Stages.Mtast.mdecl | 2 |
| Cst.cstmt | 1 |
| Stages.Ast.astmt | 1 |
| Stages.AstDB.astmt | 1 |
| **total** | 78 |

| group | arms |
|---|---|
| EnvTypes.decl | 37 |
| EnvTypes.monomorph | 8 |
| types of `lib/Cst.ml` | 14 |
| stage types of `lib/Stages.ml` | 19 |
| **total** | 78 |

Totals by file:

| file | count |
|---|---|
| lib/Env.ml | 16 |
| lib/TypeCheckExpr.ml | 9 |
| lib/Monomorphize.ml | 8 |
| lib/CombiningPass.ml | 8 |
| lib/TypingPass.ml | 5 |
| lib/TypeMatch.ml | 5 |
| lib/CstUtil.ml | 5 |
| lib/CodeGen.ml | 4 |
| lib/Entrypoint.ml | 3 |
| lib/ExtractionPass.ml | 3 |
| lib/AbstractionPass.ml | 3 |
| lib/ExportInstantiation.ml | 2 |
| lib/DesugarPaths.ml | 2 |
| lib/EnvUtils.ml | 1 |
| lib/TypeClasses.ml | 1 |
| lib/ReturnCheck.ml | 1 |
| lib/BodyExtractionPass.ml | 1 |
| lib/LinearityCheck.ml | 1 |
| **total** | 78 |

Totals by what the catch-all arm does:

| the arm | decl | monomorph | Cst | stage | total |
|---|---|---|---|---|---|
| default value | 10 | 3 | 2 | 12 | 27 |
| internal_err | 19 | 5 | 2 |  | 26 |
| named user error | 6 |  | 7 |  | 13 |
| bare err | 1 |  | 1 | 1 | 3 |
| generic handling | 1 |  | 2 | 6 | 9 |
| **total** | 37 | 8 | 14 | 19 | 78 |

"generic handling" means the arm does the general work and the named arms are the special cases.

### 3.3 The 37 arms on `EnvTypes.decl`, by how the declaration reached the match

| how | arms | rows |
|---|---|---|
| id lookup (`get_decl_by_id` result matched directly) | 11 | Env.ml:305, ExportInstantiation.ml:34, ExportInstantiation.ml:41, Monomorphize.ml:662, Monomorphize.ml:669, Monomorphize.ml:691, Monomorphize.ml:704, Monomorphize.ml:782, CodeGen.ml:450, TypeCheckExpr.ml:272, TypeCheckExpr.ml:280 |
| decl value fetched by id a few lines earlier, or returned by a helper that already narrowed it | 6 | Env.ml:383, EnvUtils.ml:70, Monomorphize.ml:217, Monomorphize.ml:678, TypeCheckExpr.ml:432, ExtractionPass.ml:526 |
| filter predicate over a list of declarations | 8 | Env.ml:260, Env.ml:270, Env.ml:281, Env.ml:290, Env.ml:350, ExtractionPass.ml:507, TypeMatch.ml:238, TypeClasses.ml:230 |
| name lookup (`get_decl_by_name` result) | 12 | Env.ml:324, Env.ml:339, Entrypoint.ml:107, Entrypoint.ml:169, Entrypoint.ml:183, Monomorphize.ml:96, ExtractionPass.ml:488, TypeCheckExpr.ml:124, TypeCheckExpr.ml:569, TypingPass.ml:56, TypingPass.ml:479, TypeMatch.ml:215 |
| **total** | 37 |  |

By the constructor the arm narrows to:

| constructor(s) expected | count |
|---|---|
| Instance | 9 |
| Function | 8 |
| Union | 5 |
| Record | 4 |
| UnionCase | 3 |
| TypeClass | 3 |
| TypeClassMethod | 2 |
| Record, UnionCase, Function, TypeClassMethod | 1 |
| Constant | 1 |
| Record, Union | 1 |
| **total** | 37 |

### 3.4 How these numbers relate to the plan's

- The plan's numbers for fragile matches are twice the number of warnings. Every number in `fragile.tsv` is even, and half of it is what reading finds (for example `lib/EnvUtils.ml` has one catch-all on `decl` and is recorded as 2). `measure.sh --fragile` builds `./lib` and `./cli`, and each module is compiled twice (bytecode and native), so each warning is printed twice. This was not confirmed by a build.
- The compiler gives one warning per match and per type that the wildcard hides. One arm can therefore count for several types: CombiningPass.ml:199 counts for `concrete_def`, `concrete_record` and `concrete_union`; TypingPass.ml:94 counts for `astmt` and `aexpr`.
- Recorded at R7, halved: `EnvTypes.decl` 37, `EnvTypes.monomorph` 8, `EnvTypes.mod_rec` 2, `EnvTypes.ins_meth_rec` 1, the `Cst` types 23, the stage types 19. Found here: 37, 8, (2, 1: see below), 14 arms that give 23 type-warnings, 18 arms that give 19 type-warnings, plus CodeGen.ml:625.
- Not in the table because the type is neither `decl` nor `monomorph`: Env.ml:88 and CodeGen.ml:448 (`Some (ModRec { ... })` then `| _ ->`, counted for `mod_rec`), Monomorphize.ml:706 (`Some (InsMethRec { ... })` then `| _ ->`, counted for `ins_meth_rec`). Both types have one constructor.
- Not counted by the compiler and not in the table: `Some d -> d | _ ->` at Env.ml:367 and Env.ml:391, where no constructor of `decl` is named.
