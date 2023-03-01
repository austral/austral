(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Types for representing Austral Compiled Module (.acm) files. *)
open Identifier
open Id
open Type
open TypeParameter
open TypeParameters
open TypeBindings
open Sexplib
open Std
open Span
open Common
open Escape
open ModuleNameSet
open Tast
open Region

(** Types in ACM files are always either public or opaque. *)
type acm_type_vis =
  | AcmTypeVisPublic
  | AcmTypeVisOpaque
[@@deriving sexp]

(** The serialized form of a statement. *)
type ser_stmt =
  | SSkip of span
  | SLet of span * identifier * ty * ser_expr * ser_stmt
  | SDestructure of span * typed_binding list * ser_expr * ser_stmt
  | SAssign of span * ser_lvalue * ser_expr
  | SIf of span * ser_expr * ser_stmt * ser_stmt
  | SCase of span * ser_expr * ser_when list
  | SWhile of span * ser_expr * ser_stmt
  | SFor of span * identifier * ser_expr * ser_expr * ser_stmt
  | SBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: ser_stmt;
      mode: borrowing_mode
    }
  | SBlock of span * ser_stmt * ser_stmt
  | SDiscarding of span * ser_expr
  | SReturn of span * ser_expr
[@@deriving sexp]

(** A reference to an instance. *)
and ins_ref =
  InsRef of {
      module_name: module_name;
      typeclass_name: identifier;
      argument: ty;
    }
[@@deriving sexp]

(** A reference to an instance method. *)
and ins_meth_ref =
  InsMethRef of ins_ref * identifier
[@@deriving sexp]

(** A reference to a named declaration. *)
and named_decl_ref = DeclRef of {
    module_name: module_name;
    decl_name: identifier;
  }

(** The serialized form of an expression. *)
and ser_expr =
  | SNilConstant
  | SBoolConstant of bool
  | SIntConstant of string
  | SFloatConstant of string
  | SStringConstant of escaped_string
  | SConstVar of qident * ty
  | SParamVar of identifier * ty
  | SLocalVar of identifier * ty
  | SFunVar of named_decl_ref * ty * type_bindings
  | SFuncall of qident * ser_expr list * ty * type_bindings
  | SMethodCall of ins_meth_ref * typarams * ser_expr list * ty * type_bindings
  | SVarMethodCall of {
      method_name: sident;
      args: ser_expr list;
      dispatch_ty: ty;
      rt: ty;
    }
  | SFptrCall of identifier * ser_expr list * ty
  | SCast of ser_expr * ty
  | SComparison of comparison_operator * ser_expr * ser_expr
  | SConjunction of ser_expr * ser_expr
  | SDisjunction of ser_expr * ser_expr
  | SNegation of ser_expr
  | SIfExpression of ser_expr * ser_expr * ser_expr
  | SRecordConstructor of ty * (identifier * ser_expr) list
  | SUnionConstructor of ty * identifier * (identifier * ser_expr) list
  | SPath of {
      head: ser_expr;
      elems: ser_path_elem list;
      ty: ty
    }
  | SEmbed of ty * string * ser_expr list
  | SDeref of ser_expr
  | SSizeOf of ty
  | SBorrowExpr of borrowing_mode * identifier * region * ty
[@@deriving sexp]

and ser_when =
  SerWhen of identifier * typed_binding list * ser_stmt
[@@deriving sexp]

and ser_path_elem =
  | SerSlotAccessor of identifier * ty
  | SerPointerSlotAccessor of identifier * ty
  | SerArrayIndex of ser_expr * ty
[@@deriving sexp]

and ser_lvalue =
  | SerLValue of identifier * ser_path_elem list
[@@deriving sexp]

and ser_arglist =
  | SerPositionalArglist of ser_expr list
  | SerNamedArglist of (identifier * ser_expr) list
[@@deriving sexp]

type compiled_decl =
  | CompiledConstant of {
      name: identifier;
      ty: ty;
    }
  (** Constants in ACM files are always public. *)
  | CompiledRecord of {
      vis: acm_type_vis;
      name: identifier;
      typarams: typarams;
      universe: universe;
      slots: typed_slot list;
    }
  | CompiledUnion of {
      vis: acm_type_vis;
      name: identifier;
      typarams: typarams;
      universe: universe;
      cases: typed_case list;
    }
  | CompiledFunction of {
      name: identifier;
      typarams: typarams;
      value_params: value_parameter list;
      rt: ty;
      external_name: string option;
      (** If this function is foreign, this is the name of the underlying function
          that will be called. *)
      body: ser_stmt option;
      (** If the function is generic, it should have a body. *)
    }
  (** Functions in ACM files are always public. *)
  | CompiledTypeClass of {
      mod_id: mod_id;
      name: identifier;
      param: type_parameter;
      methods: compiled_method_decl list;
    }
  (** Type classes in ACM files are always public. *)
  | CompiledInstance of {
      typeclass_name: sident;
      typarams: typarams;
      argument: ty;
      methods: compiled_method_def list;
    }
  (** Instances in ACM files are always public. *)

and compiled_method_decl =
  CompiledMethodDecl of {
      name: identifier;
      value_params: value_parameter list;
      rt: ty;
    }

and compiled_method_def =
  CompiledMethodDef of {
      name: identifier;
      value_params: value_parameter list;
      rt: ty;
      body: ser_stmt;
    }
[@@deriving sexp]

(** Represents the contents of an ACM file. *)
type compiled_module =
  CompiledModule of {
      name: module_name;
      imports_from: ModuleNameSet.t;
      decls: compiled_decl list;
    }
