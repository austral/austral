open Identifier
open Common
open Span
open Escape

type qtypespec = QTypeSpecifier of qident * qtypespec list
[@@deriving show]

type astmt =
  | ASkip of span
  | ALet of span * identifier * qtypespec * aexpr * astmt
  | ADestructure of span * (identifier * qtypespec) list * aexpr * astmt
  | AAssign of span * lvalue * aexpr
  | AIf of span * aexpr * astmt * astmt
  | AWhen of span * aexpr * astmt
  | ACase of span * aexpr * abstract_when list
  | AWhile of span * aexpr * astmt
  | AFor of {
      span: span;
      name: identifier;
      initial: aexpr;
      final: aexpr;
      body: astmt
    }
  | ABorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      body: astmt;
      mode: borrowing_mode
    }
  | ABlock of span * astmt * astmt
  | ADiscarding of span * aexpr
  | AReturn of span * aexpr

and aexpr =
  | NilConstant
  | BoolConstant of bool
  | IntConstant of string
  | FloatConstant of string
  | StringConstant of escaped_string
  | Variable of qident
  | FunctionCall of qident * abstract_arglist
  | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
  | Comparison of comparison_operator * aexpr * aexpr
  | Conjunction of aexpr * aexpr
  | Disjunction of aexpr * aexpr
  | Negation of aexpr
  | IfExpression of aexpr * aexpr * aexpr
  | Path of aexpr * path_elem list
  | Embed of qtypespec * string * aexpr list
  | Deref of aexpr
  | Typecast of aexpr * qtypespec
  | SizeOf of qtypespec

and abstract_when =
  | AbstractWhen of identifier * (identifier * qtypespec) list * astmt

and abstract_arglist =
  | Positional of aexpr list
  | Named of (identifier * aexpr) list

and path_elem =
  | SlotAccessor of identifier
  | PointerSlotAccessor of identifier
  | ArrayIndex of aexpr

and lvalue =
  LValue of identifier * path_elem list

let expr_kind (expr: aexpr): string =
  match expr with
  | NilConstant ->
     "nil constant"
  | BoolConstant _ ->
     "bool constant"
  | IntConstant _ ->
     "int constant"
  | FloatConstant _ ->
     "float constant"
  | StringConstant _ ->
     "string constant"
  | Variable _ ->
     "variable"
  | FunctionCall _ ->
     "function call"
  | ArithmeticExpression _ ->
     "arithmetic"
  | Comparison _ ->
     "comparison"
  | Conjunction _ ->
     "conjunction"
  | Disjunction _ ->
     "disjunction"
  | Negation _ ->
     "negation"
  | IfExpression _ ->
     "if"
  | Path _ ->
     "path"
  | Embed _ ->
     "embed"
  | Deref _ ->
     "dereference"
  | Typecast _ ->
     "typecast"
  | SizeOf _ ->
     "sizeof"
