open Identifier
open Common

type qtypespec = QTypeSpecifier of qident * qtypespec list

type astmt =
  | ASkip
  | ALet of {
      name: identifier;
      ty: qtypespec;
      value: aexpr
    }
  | AAssign of identifier * aexpr
  | AIf of aexpr * astmt * astmt
  | ACase of aexpr * abstract_when list
  | AWhile of aexpr * astmt
  | AFor of {
      name: identifier;
      initial: aexpr;
      final: aexpr;
      body: astmt
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
  | Variable of identifier
  | FunctionCall of qident * abstract_arglist
  | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
  | Comparison of comparison_operator * aexpr * aexpr
  | Conjunction of aexpr * aexpr
  | Disjunction of aexpr * aexpr
  | Negation of aexpr
  | IfExpression of aexpr * aexpr * aexpr

and abstract_when =
  | AbstractWhen of {
      name: identifier;
      bindings: (identifier * qtypespec) list;
      body: astmt
    }

and abstract_arglist =
  | Positional of aexpr list
  | Named of (identifier * aexpr) list
