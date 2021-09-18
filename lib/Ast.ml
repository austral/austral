open Identifier
open Common

type qtypespec = QTypeSpecifier of qident * qtypespec list

type astmt =
  | ASkip
  | ALet of identifier * qtypespec * aexpr * astmt
  | ADestructure of (identifier * qtypespec) list * aexpr * astmt
  | AAssign of lvalue * aexpr
  | AIf of aexpr * astmt * astmt
  | ACase of aexpr * abstract_when list
  | AWhile of aexpr * astmt
  | AFor of {
      name: identifier;
      initial: aexpr;
      final: aexpr;
      body: astmt
    }
  | ABorrow of {
      original: identifier;
      rename: identifier;
      region: identifier;
      body: astmt;
      mode: borrowing_mode
    }
  | ABlock of astmt * astmt
  | ADiscarding of aexpr
  | AReturn of aexpr

and aexpr =
  | NilConstant
  | BoolConstant of bool
  | IntConstant of string
  | FloatConstant of string
  | StringConstant of string
  | Variable of qident
  | FunctionCall of qident * abstract_arglist
  | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
  | Comparison of comparison_operator * aexpr * aexpr
  | Conjunction of aexpr * aexpr
  | Disjunction of aexpr * aexpr
  | Negation of aexpr
  | IfExpression of aexpr * aexpr * aexpr
  | Path of aexpr * path_elem list
  | PathRef of aexpr * path_elem list

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
