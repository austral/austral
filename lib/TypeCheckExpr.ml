(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open BuiltIn
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
open TypeBindings
open TypeMatch
open Error

module Errors = TypeErrors

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

let ctx_module_name (ctx: expr_ctx): module_name =
  let (ExpressionContext { module_name; _ }) = ctx in
  module_name

let ctx_env (ctx: expr_ctx): env =
  let (ExpressionContext { env; _ }) = ctx in
  env

let ctx_lexenv (ctx: expr_ctx): lexenv =
  let (ExpressionContext { lexenv; _ }) = ctx in
  lexenv

(* Utilities *)

(** Wrapper around match_type. *)
let match_type_ctx (ctx: expr_ctx) (expected: ty) (actual: ty): type_bindings =
  let env: env = ctx_env ctx
  and module_name: module_name = ctx_module_name ctx in
  match_type (env, module_name) expected actual

(** Wrapper around match_type_with_value. *)
let match_type_with_value_ctx (ctx: expr_ctx) (expected: ty) (value: texpr): type_bindings =
  let env: env = ctx_env ctx
  and module_name: module_name = ctx_module_name ctx in
  match_type_with_value (env, module_name) expected value

let get_path_ty_from_elems (elems: typed_path_elem list): ty =
  assert ((List.length elems) > 0);
  let last = List.nth elems ((List.length elems) - 1) in
  path_elem_type last

(** Is this expression of boolean type? *)
let is_bool (e: texpr): bool =
  match get_type e with
  | Boolean -> true
  | _ -> false

(** Is this expression an integer constant? *)
let is_int_constant (e: aexpr): bool =
  match e with
  | IntConstant _ -> true
  | _ -> false

(** Is this expression a float constant? *)
let is_float_constant (e: aexpr): bool =
  match e with
  | FloatConstant _ -> true
  | _ -> false

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type env [] rm typarams ty

(* Interface *)

let rec augment_expr (ctx: expr_ctx) (asserted_ty: ty option) (expr: aexpr): texpr =
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
  | Variable name ->
     augment_variable ctx name asserted_ty
  | FunctionCall _ ->
     internal_err "Not implemented yet"
  | ArithmeticExpression _ ->
     internal_err "Not implemented yet"
  | Comparison (op, lhs, rhs) ->
     augment_comparison ctx op lhs rhs
  | Conjunction (lhs, rhs) ->
     augment_conjunction ctx lhs rhs
  | Disjunction (lhs, rhs) ->
     augment_disjunction ctx lhs rhs
  | Negation e ->
     augment_negation ctx e
  | IfExpression (c, t, f) ->
     augment_if_expr ctx c t f
  | _ ->
     internal_err "Not implemented yet"

and augment_variable (ctx: expr_ctx) (name: qident) (asserted_ty: ty option): texpr =
  let env = ctx_env ctx in
  (* Does this name a declaration? *)
  match get_decl_by_name env (qident_to_sident name) with
  | Some (Function { id; typarams; value_params; rt; _ }) ->
     let arg_tys: ty list = List.map (fun (ValueParameter (_, ty)) -> ty) value_params in
     let fn_ty: ty = FnPtr (arg_tys, rt) in
     if (typarams_size typarams > 0) then
       (* Function is generic, need an asserted type. *)
       (match asserted_ty with
        | Some asserted_ty ->
           let bindings: type_bindings = match_type_ctx ctx fn_ty asserted_ty in
           let rt: ty = replace_variables bindings rt
           and arg_tys: ty list = List.map (replace_variables bindings) arg_tys in
           let effective_fn_ty: ty = FnPtr (arg_tys, rt) in
           TFunVar (id, effective_fn_ty, bindings)
        | None ->
           Errors.unconstrained_generic_function (original_name name))
     else
       (* Function is concrete. *)
       TFunVar (id, fn_ty, empty_bindings)
  | _ ->
     (match get_variable (ctx_env ctx) (ctx_lexenv ctx) name with
      | Some (ty, src) ->
         (match src with
          | VarConstant ->
             TConstVar (name, ty)
          | VarParam ->
             TParamVar ((original_name name), ty)
          | VarLocal ->
             TLocalVar ((original_name name), ty))
      | None ->
         Errors.unknown_name
           ~kind:"variable"
           ~name:(original_name name))

and augment_comparison ctx op lhs rhs =
  let lhs' = aug ctx lhs
  and rhs' = aug ctx rhs in
  let _ = match_type_with_value_ctx ctx (get_type lhs') rhs' in
  TComparison (op, lhs', rhs')

and augment_conjunction ctx lhs rhs =
  let lhs': texpr = aug ctx lhs
  and rhs': texpr = aug ctx rhs in
  if ((is_bool lhs') && (is_bool rhs')) then
    TConjunction (lhs', rhs')
  else
    Errors.logical_operands_not_boolean
      ~operator:"conjunction"
      ~types:[get_type lhs'; get_type rhs']

and augment_disjunction ctx lhs rhs =
  let lhs': texpr = aug ctx lhs
  and rhs': texpr = aug ctx rhs in
  if ((is_bool lhs') && (is_bool rhs')) then
    TDisjunction (lhs', rhs')
  else
    Errors.logical_operands_not_boolean
      ~operator:"disjunction"
      ~types:[get_type lhs'; get_type rhs']

and augment_negation ctx e =
  let e' = aug ctx e in
  if is_bool e' then
    TNegation e'
  else
    Errors.logical_operands_not_boolean
      ~operator:"negation"
      ~types:[get_type e']

and augment_if_expr ctx c t f =
  let c' = aug ctx c
  and t' = aug ctx t
  and f' = aug ctx f in
  if is_bool c' then
    if (get_type t') = (get_type f') then
      TIfExpression (c', t', f')
    else
      Errors.if_inequal
        ~lhs:(get_type t')
        ~rhs:(get_type f')
  else
    Errors.condition_not_boolean
      ~kind:"if"
      ~form:"expression"
      ~ty:(get_type c')

(* Further utilities, these have to be defined here because of `let rec and`
   bullshit. *)

(** `aug` is a shorthand for `augment_expr` with `None` as the asserted type. *)
and aug (ctx: expr_ctx) (expr: aexpr): texpr =
  augment_expr ctx None expr
