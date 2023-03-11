(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Common
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

let ctx_region_map (ctx: expr_ctx): region_map =
  let (ExpressionContext { rm; _ }) = ctx in
  rm

let ctx_typarams (ctx: expr_ctx): typarams =
  let (ExpressionContext { typarams; _ }) = ctx in
  typarams

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

(** Wrapper around match_typarams_ctx. *)
let match_typarams_ctx (ctx: expr_ctx) (typarams: typarams) (type_args: ty list): type_bindings =
  match_typarams (ctx_env ctx, ctx_module_name ctx) typarams type_args

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
let parse_typespec (ctx: expr_ctx) (ty: qtypespec): ty =
  parse_type (ctx_env ctx) [] (ctx_region_map ctx) (ctx_typarams ctx) ty

let get_record_definition (env: env) (name: qident): (module_name * type_vis * typarams * typed_slot list) =
  match get_decl_by_name env (qident_to_sident name) with
  | (Some (Record { mod_id; vis; typarams; slots; _ })) ->
     let mod_name: module_name = module_name_from_id env mod_id in
     (mod_name, vis, typarams, slots)
  | Some _ ->
     Errors.path_not_record (original_name name |> ident_string)
  | None ->
     err ("No record with this name: " ^ (ident_string (original_name name)))

let get_slot_with_name type_name slots slot_name =
  match List.find_opt (fun (TypedSlot (n, _)) -> n = slot_name) slots with
  | Some s -> s
  | None ->
     Errors.no_such_slot
       ~type_name:(original_name type_name)
       ~slot_name

(* Type Checking *)

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
  | Path (e, elems) ->
     augment_path_expr ctx e elems
  | Deref expr ->
     augment_deref ctx expr
  | Typecast (expr, ty) ->
     augment_typecast ctx expr ty
  | SizeOf ty ->
     TSizeOf (parse_typespec ctx ty)
  | BorrowExpr (mode, name) ->
     augment_borrow_expr ctx mode name
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

and augment_path_expr (ctx: expr_ctx) (e: aexpr) (elems: path_elem list): texpr =
  (* Path rules:

     1. Path that begins in a reference ends in a reference.
     2. All paths end in a type in the free universe.
   *)
  let e': texpr = aug ctx e in
  (* Is the initial expression of a path a read reference or write reference or
     no reference? *)
  let (is_read, region): (bool * ty option) =
    (match (get_type e') with
     | ReadRef (_, r) ->
        (true, Some r)
     | WriteRef (_, r) ->
        (false, Some r)
     | _ ->
        (false, None))
  in
  let elems' = augment_path ctx (get_type e') elems in
  let path_ty: ty = get_path_ty_from_elems elems' in
  let path_ty: ty =
    (* If the path starts in a reference it should end in one. *)
    (match region with
     | Some reg ->
        if is_read then
          ReadRef (path_ty, reg)
        else
          WriteRef (path_ty, reg)
     | None ->
        path_ty)
  in
  let universe = type_universe path_ty in
  if universe = FreeUniverse then
    TPath {
        head = e';
        elems = elems';
        ty = path_ty
      }
  else
    Errors.path_not_free path_ty

and augment_path (ctx: expr_ctx) (head_ty: ty) (elems: path_elem list): typed_path_elem list =
  match elems with
  | [elem] ->
     [augment_path_elem ctx head_ty elem]
  | elem::rest ->
     let elem' = augment_path_elem ctx head_ty elem in
     let rest' = augment_path ctx (path_elem_type elem') rest in
     elem' :: rest'
  | [] ->
     err "Path is empty"

and augment_path_elem (ctx: expr_ctx) (head_ty: ty) (elem: path_elem): typed_path_elem =
  match elem with
  | SlotAccessor slot_name ->
     (match head_ty with
      | NamedType (name, args, _) ->
         augment_slot_accessor_elem ctx slot_name name args
      | _ ->
         Errors.path_not_record (type_string head_ty))
  | PointerSlotAccessor slot_name ->
     (match head_ty with
      | Pointer pointed_to ->
         (* TODO: Addresses should not be indexable. *)
         augment_pointer_slot_accessor_elem ctx slot_name pointed_to
      | ReadRef (ty, _) ->
         (match ty with
          | NamedType (name, args, _) ->
             augment_reference_slot_accessor_elem ctx slot_name name args
          | _ ->
             Errors.path_not_record (type_string ty))
      | WriteRef (ty, _) ->
         (match ty with
          | NamedType (name, args, _) ->
             augment_reference_slot_accessor_elem ctx slot_name name args
          | _ ->
             Errors.path_not_record (type_string ty))
      | _ ->
         Errors.path_not_record (type_string head_ty))
  | ArrayIndex ie ->
     let ie' = aug ctx ie in
     (match head_ty with
      | StaticArray elem_ty ->
         TArrayIndex (ie', elem_ty)
      | _ ->
         Errors.array_indexing_disallowed head_ty)

and augment_slot_accessor_elem (ctx: expr_ctx) (slot_name: identifier) (type_name: qident) (type_args: ty list) =
  let module_name: module_name = ctx_module_name ctx in
  (* Check: e' is a public record type *)
  let (source_module, vis, typarams, slots) = get_record_definition (ctx_env ctx) type_name in
  if (vis = TypeVisPublic) || (module_name = source_module) then
    (* Check: the given slot name must exist in this record type. *)
    let (TypedSlot (_, slot_ty)) = get_slot_with_name type_name slots slot_name in
    let bindings = match_typarams_ctx ctx typarams type_args in
    let slot_ty' = replace_variables bindings slot_ty in
    TSlotAccessor (slot_name, slot_ty')
  else
    Errors.path_not_public
      ~type_name:(original_name type_name)
      ~slot_name

and augment_pointer_slot_accessor_elem (ctx: expr_ctx) (slot_name: identifier) (pointed_to: ty): typed_path_elem =
  let module_name: module_name = ctx_module_name ctx in
  match pointed_to with
  | NamedType (type_name, type_args, _) ->
     (* Check arg is a public record *)
     let (source_module, vis, typarams, slots) = get_record_definition (ctx_env ctx) type_name in
     if (vis = TypeVisPublic) || (module_name = source_module) then
       (* Check: the given slot name must exist in this record type. *)
       let (TypedSlot (_, slot_ty)) = get_slot_with_name type_name slots slot_name in
       let bindings = match_typarams_ctx ctx typarams type_args in
       let slot_ty' = replace_variables bindings slot_ty in
       TPointerSlotAccessor (slot_name, slot_ty')
     else
       Errors.path_not_public
         ~type_name:(original_name type_name)
         ~slot_name
  | _ ->
     Errors.path_not_record (type_string pointed_to)

and augment_reference_slot_accessor_elem (ctx: expr_ctx) (slot_name: identifier) (type_name: qident) (type_args: ty list) =
  let module_name: module_name = ctx_module_name ctx in
  (* Check: e' is a public record type *)
  let (source_module, vis, typarams, slots) = get_record_definition (ctx_env ctx) type_name in
  if (vis = TypeVisPublic) || (module_name = source_module) then
    (* Check: the given slot name must exist in this record type. *)
    let (TypedSlot (_, slot_ty)) = get_slot_with_name type_name slots slot_name in
    let bindings = match_typarams_ctx ctx typarams type_args in
    let slot_ty' = replace_variables bindings slot_ty in
    TPointerSlotAccessor (slot_name, slot_ty')
  else
    Errors.path_not_public
      ~type_name:(original_name type_name)
      ~slot_name

and augment_deref (ctx: expr_ctx) (expr: aexpr): texpr =
  (* The type of the expression being dereferenced must be either a read-only
     reference or a write reference. *)
  let expr' = aug ctx expr in
  let ty = get_type expr' in
  (match ty with
   | ReadRef _ ->
      TDeref expr'
   | WriteRef _ ->
      TDeref expr'
   | _ ->
      Errors.dereference_non_reference ty)

and augment_typecast (ctx: expr_ctx) (expr: aexpr) (ty: qtypespec): texpr =
  (* The typecast operator has four uses:

     1. Clarifying the type of integer and floating point literals.

     2. Converting write references to read references.

     3. Clarifying the type of return type polymorphic functions.

   *)
  let is_int_expr = function
    | TIntConstant _ -> true
    | _ -> false
  and is_float_expr = function
    | TFloatConstant _ -> true
    | _ -> false

  and is_int_type = function
    | Integer _ -> true
    | _ -> false

  and is_float_type = function
    | SingleFloat -> true
    | DoubleFloat -> true
    | _ -> false
  in
  let target_type = parse_typespec ctx ty in
  (* By passing target_type as the asserted type we're doing point 3. *)
  let expr' = augment_expr ctx (Some target_type) expr in
  (* For 1: if the expression is an int literal and the target type is an
     int type, do the conversion. Similarly, if the expression is a float
     constant and the target type is a float, do the conversion. *)
  if (is_int_expr expr') && (is_int_type target_type) then
    TCast (expr', target_type)
  else
    if (is_float_expr expr') && (is_float_type target_type) then
      TCast (expr', target_type)
    else
      (* Otherwise, if the expression has a mutable reference type and the
         target type is a read reference, and they point to the same underlying
         type and underlying region, just convert it to a read ref. This does
         point 2. *)
      (match (get_type expr') with
       | WriteRef (underlying_ty, region) ->
          (match target_type with
           | ReadRef (underlying_ty', region') ->
              if ((equal_ty underlying_ty underlying_ty') && (equal_ty region region')) then
                TCast (expr', target_type)
              else
                Errors.cast_different_references
                  ~different_types:(not (equal_ty underlying_ty underlying_ty'))
                  ~different_regions:(not (equal_ty region region'))
           | _ ->
              Errors.cast_write_ref_to_non_ref ())
       | _ ->
          (try
             let _ = match_type_ctx ctx target_type (get_type expr') in
             expr'
           with Austral_error _ ->
             Errors.cast_invalid
               ~target:target_type
               ~source:(get_type expr')))

and augment_borrow_expr (ctx: expr_ctx) (mode: borrowing_mode) (name: qident): texpr =
  (match get_variable (ctx_env ctx) (ctx_lexenv ctx) name with
   | Some (ty, src) ->
      (* TODO: check if `ty` is linear? *)
      (match src with
       | VarConstant ->
          Errors.borrow_constant ()
       | VarParam ->
          let name: identifier = original_name name
          and reg: region = fresh_region ()
          in
          TBorrowExpr (mode, name, reg, ty)
       | VarLocal ->
          let name: identifier = original_name name
          and reg: region = fresh_region ()
          in
          TBorrowExpr (mode, name, reg, ty))
   | None ->
      Errors.unknown_name
        ~kind:"variable"
        ~name:(original_name name))


(* Further utilities, these have to be defined here because of `let rec and`
   bullshit. *)

(** `aug` is a shorthand for `augment_expr` with `None` as the asserted type. *)
and aug (ctx: expr_ctx) (expr: aexpr): texpr =
  augment_expr ctx None expr
