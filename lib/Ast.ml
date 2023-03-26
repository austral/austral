(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Span
open Escape

type qtypespec =
  | QTypeSpecifier of qident * qtypespec list
  | QReadRef of qtypespec * qtypespec
  | QWriteRef of qtypespec * qtypespec
[@@deriving show]

type qbinding =
  QBinding of {
      name: identifier;
      ty: qtypespec;
      rename: identifier;
    }

type astmt =
  | ASkip of span
  | ALet of span * identifier * qtypespec * aexpr * astmt
  | ADestructure of span * qbinding list * aexpr * astmt
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
  | RefPath of aexpr * ref_path_elem list
  | Embed of qtypespec * string * aexpr list
  | Deref of aexpr
  | Typecast of aexpr * qtypespec
  | SizeOf of qtypespec
  | BorrowExpr of borrowing_mode * qident

and abstract_when =
  | AbstractWhen of identifier * qbinding list * astmt

and abstract_arglist =
  | Positional of aexpr list
  | Named of (identifier * aexpr) list

and path_elem =
  | SlotAccessor of identifier
  | PointerSlotAccessor of identifier
  | ArrayIndex of aexpr

and ref_path_elem =
  | RefSlotAccessor of identifier

and lvalue =
  LValue of identifier * path_elem list
