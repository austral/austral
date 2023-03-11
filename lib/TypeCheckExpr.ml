(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Env
open Region
open Type
open TypeSystem
open TypeParser
open Ast
open Tast
open TastUtil
open TypeParameters
open LexEnv
open Error

(* Expression Context *)

type expr_ctx =
  ExpressionContext of {
      module_name: module_name;
      (** The name of the module being type-checked. *)
      env: env;
      (** The environment. *)
      rm: region_map;
      (** The region map, mapping the names of regions defined in
          borrow statements to region objects. *)
      typarams: typarams;
      (** Type parameters of the function or method containing this expression. *)
      lexenv: lexenv;
      (** The lexical environment. *)
    }

(* Utilities *)

let get_path_ty_from_elems (elems: typed_path_elem list): ty =
  assert ((List.length elems) > 0);
  let last = List.nth elems ((List.length elems) - 1) in
  path_elem_type last

let is_bool (e: texpr): bool =
  match get_type e with
  | Boolean -> true
  | _ -> false

let is_int_constant (e: aexpr): bool =
  match e with
  | IntConstant _ -> true
  | _ -> false

let is_float_constant (e: aexpr): bool =
  match e with
  | FloatConstant _ -> true
  | _ -> false

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type env [] rm typarams ty

(* Type checking expressions *)

(* Interface *)

let rec augment_expr (ctx: expr_ctx) (asserted_ty: ty option) (expr: aexpr): texpr =
  let _ = (ctx, asserted_ty, expr) in
  (* `aug` is a shortmhand for `augment_expr` with `None` as the asserted type. *)
  let aug = augment_expr ctx None in
  (* Dispatch on the AST *)
  match expr with
  | NilConstant ->
     TNilConstant
  | BoolConstant b ->
     TBoolConstant b
  | IntConstant i ->
     TIntConstant i
  | FloatConstant f ->
     TFloatConstant f
  | StringConstant s ->
     TStringConstant s
  | _ ->
     internal_err "Not implemented yet"
