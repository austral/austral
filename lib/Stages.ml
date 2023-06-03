(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Compiler stages. *)

(** The AST is the CST, but imports are qualified and `let` statements are
    reshaped. *)
module Ast = struct
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
    | ALet of span * mutability * identifier * qtypespec * aexpr * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
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
        mode: borrow_stmt_kind
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
    | Reborrow of qident

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
end

(** The combined representation merges the interface and body. *)
module Combined = struct
  open Identifier
  open Common
  open Imports
  open Ast
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * combined_method_decl list * docstring
    | CInstance of span * vis * qident * typarams * qtypespec * combined_method_def list * docstring

  and qslot = QualifiedSlot of identifier * qtypespec

  and qcase = QualifiedCase of identifier * qslot list

  and qparam = QualifiedParameter of identifier * qtypespec

  and combined_method_decl = CMethodDecl of identifier * typarams * qparam list * qtypespec * docstring

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end

(** The AST, but anonymous borrows are desugard. *)
module AstDB = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * aexpr * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
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
        mode: borrow_stmt_kind
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
end

(** The combined representation, but bodies are thoroughly desugared. *)
module SmallCombined = struct
  open Identifier
  open Common
  open Imports
  open AstDB
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type qslot = Combined.qslot
  type qcase = Combined.qcase
  type qparam = Combined.qparam

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot
 list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * Combined.combined_method_decl list * docstring
    | CInstance of span * vis * qident * typarams * qtypespec * combined_method_def list * docstring

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end
