(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Common
open Escape
open Type
open TypeParameter
open TypeParameters
open TypeBindings
open Span
open Linked
open Id

type typed_binding =
  TypedBinding of {
      name: identifier;
      ty: ty;
      rename: identifier;
    }
[@@deriving (show, sexp)]

type case_ref =
  | CasePlain
  | CaseRef
[@@deriving (show, sexp)]

type tstmt =
  | TSkip of span
  | TLet of span * mutability * identifier * ty * texpr * tstmt
  | TDestructure of span * mutability * typed_binding list * texpr * tstmt
  | TAssign of span * typed_lvalue * texpr
  | TIf of span * texpr * tstmt * tstmt
  | TCase of span * texpr * typed_when list * case_ref
  | TWhile of span * texpr * tstmt
  | TFor of span * identifier * texpr * texpr * tstmt
  | TBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: tstmt;
      mode: borrow_stmt_kind
    }
  | TBlock of span * tstmt * tstmt
  | TDiscarding of span * texpr
  | TReturn of span * texpr
[@@deriving show]

and texpr =
  | TNilConstant
  | TBoolConstant of bool
  | TIntConstant of string
  | TFloatConstant of string
  | TStringConstant of escaped_string
  | TConstVar of qident * ty
  | TParamVar of identifier * ty
  | TLocalVar of identifier * ty
  | TFunVar of decl_id * ty * type_bindings
  (** Represents accessing a function as a value. *)
  | TFuncall of decl_id * qident * texpr list * ty * type_bindings
  | TMethodCall of ins_meth_id * qident * typarams * texpr list * ty * type_bindings
  | TVarMethodCall of {
      source_module_name: module_name;
      typeclass_id: decl_id;
      params: value_parameter list;
      method_name: qident;
      args: texpr list;
      dispatch_ty: ty;
      rt: ty;
      bindings: type_bindings;
    }
  | TFptrCall of identifier * texpr list * ty
  | TCast of texpr * ty
  | TComparison of comparison_operator * texpr * texpr
  | TConjunction of texpr * texpr
  | TDisjunction of texpr * texpr
  | TNegation of texpr
  | TIfExpression of texpr * texpr * texpr
  | TRecordConstructor of ty * (identifier * texpr) list
  | TUnionConstructor of ty * identifier * (identifier * texpr) list
  | TPath of {
      head: texpr;
      elems: typed_path_elem list;
      ty: ty
    }
  | TRefPath of texpr * typed_ref_path_elem list * ty
  | TEmbed of ty * string * texpr list
  | TDeref of texpr
  | TSizeOf of ty
[@@deriving show]

and typed_when =
  TypedWhen of identifier * typed_binding list * tstmt
[@@deriving show]

and typed_path_elem =
  | TSlotAccessor of identifier * ty
  | TPointerSlotAccessor of identifier * ty
  | TArrayIndex of texpr * ty
[@@deriving show]

and typed_ref_path_elem =
  | TRefSlotAccessor of identifier * ty
[@@deriving show]

and typed_lvalue =
  TypedLValue of identifier * typed_path_elem list
[@@deriving show]

and typed_arglist =
  | TPositionalArglist of texpr list
  | TNamedArglist of (identifier * texpr) list
[@@deriving show]

type typed_method_decl =
  TypedMethodDecl of decl_id * identifier * value_parameter list * ty

type typed_method_def =
  TypedMethodDef of ins_meth_id * identifier * value_parameter list * ty * tstmt

type typed_decl =
  | TConstant of decl_id * vis * identifier * ty * texpr * docstring
  | TRecord of decl_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
  | TUnion of decl_id * type_vis * identifier * typarams * universe * linked_case list * docstring
  | TFunction of decl_id * vis * identifier * typarams * value_parameter list * ty * tstmt * docstring
  | TForeignFunction of decl_id * vis * identifier * value_parameter list * ty * string * docstring
  | TTypeClass of decl_id * vis * identifier * type_parameter * typed_method_decl list * docstring
  | TInstance of decl_id * vis * qident * typarams * ty * typed_method_def list * docstring

type typed_module = TypedModule of module_name * typed_decl list
