type arithmetic_operator =
  | Add
  | Subtract
  | Multiply
  | Divide

type comparison_operator =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

type docstring = Docstring of string

type pragma =
  | ForeignImportPragma of string
  | UnsafeModulePragma

type borrowing_mode =
  | ReadBorrow
  | WriteBorrow

(** Whether a module is safe or unsafe. *)
type module_kind =
  | SafeModule
  | UnsafeModule

(** The visibility of a type: public, opaque, or private. *)
type type_vis =
  | TypeVisPublic
  | TypeVisOpaque
  | TypeVisPrivate

(** The visibility of another declaration: public or private. *)
type vis =
  | VisPublic
  | VisPrivate
