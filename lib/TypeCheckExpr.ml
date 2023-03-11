(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Env
open Region
open Type
open TypeParser
open Ast
open Tast
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

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type env [] rm typarams ty

(* Type checking expressions *)

(* Interface *)

let augment_expr (ctx: expr_ctx) (asserted_ty: ty option) (expr: aexpr): texpr =
  let _ = (ctx, asserted_ty, expr) in
  internal_err "Not implemented yet"
