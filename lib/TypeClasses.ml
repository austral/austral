(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open IdentifierSet
open Type
open TypeParameters
open TypeParser
open TypeSystem
open Env
open EnvTypes
open EnvExtras
open EnvUtils
open Error

let check_instance_argument_has_right_universe (universe: universe) (arg: ty): unit =
  if universe_compatible universe (type_universe arg) then
    ()
  else
    type_error (InstanceArgumentWrongUniverse {
        ty = arg;
        expected = universe;
        actual = type_universe arg;
      })

let check_instance_argument_has_right_shape (typarams: typarams) (arg: ty): unit =
  let is_tyvar (ty: ty): type_var =
    match ty with
    | TyVar tv ->
       tv
    | _ ->
       declaration_error (InstanceArgumentNotATyvar { ty; universe = type_universe ty })
  and all_distinct (tyvars: type_var list): unit =
    let names: identifier list = List.map (fun (TypeVariable (name, _, _, _)) -> name) tyvars in
    let set: IdentifierSet.t = IdentifierSet.of_list names in
    if List.length tyvars = IdentifierSet.cardinal set then
      ()
    else
      declaration_error InstanceDuplicateTyvars
  (* Assert the number of type parameters *)
  and no_leftovers (n: int): unit =
    if (typarams_size typarams) = n then
      ()
    else
      declaration_error InstanceLeftoverTypeParameters
  in
  match arg with
  | Unit -> ()
  | Boolean -> ()
  | Integer _ -> ()
  | SingleFloat -> ()
  | DoubleFloat -> ()
  | NamedType (_, args, _) ->
     let _ = all_distinct (List.map is_tyvar args) in
     let _ = no_leftovers (List.length args) in
     ()
  | RegionTy _ ->
     declaration_error InstanceArgumentRegion
  | ReadRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | WriteRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | Span (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | SpanMut (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | TyVar _ ->
     declaration_error InstanceArgumentLoneTyvar
  | Address ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | Pointer ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | FnPtr (args, ty) ->
     let args' = ty :: args in
     let _ = all_distinct (List.map is_tyvar args') in
     let _ = no_leftovers (List.length args') in
     ()
  | MonoTy _ ->
     generic_error InstanceArgumentShapeNotAllowed

let overlapping_instances (a: ty) (b: ty): bool =
  match a with
  | Unit ->
     (match b with
      | Unit -> true
      | _ -> false)
  | Boolean ->
     (match b with
      | Boolean -> true
      | _ -> false)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         (equal_signedness s s')
         && (equal_integer_width w w')
      | _ ->
         false)
  | SingleFloat ->
     (match b with
      | SingleFloat -> true
      | _ -> false)
  | DoubleFloat ->
     (match b with
      | DoubleFloat -> true
      | _ -> false)
  | NamedType (name, _, _) ->
     (match b with
      | NamedType (name', _, _) ->
         equal_qident name name'
      | _ ->
         false)
  | RegionTy _ ->
     generic_error InstanceOverlapTypeNotAllowed
  | ReadRef _ ->
     (match b with
      | ReadRef _ -> true
      | _ -> false)
  | WriteRef _ ->
     (match b with
      | WriteRef _ -> true
      | _ -> false)
  | Span _ ->
     (match b with
      | Span _ -> true
      | _ -> false)
  | SpanMut _ ->
     (match b with
      | SpanMut _ -> true
      | _ -> false)
  | TyVar _ ->
     generic_error InstanceOverlapTypeNotAllowed
  | Address _ ->
     (match b with
      | Address _ -> true
      | _ -> false)
  | Pointer _ ->
     (match b with
      | Pointer _ -> true
      | _ -> true)
  | FnPtr _ ->
     (match b with
      | FnPtr _ -> true
      | _ -> false)
  | MonoTy _ ->
     generic_error InstanceOverlapTypeNotAllowed

let check_instance_locally_unique (instances: decl list) (argument: ty): unit =
  let pred (decl: decl): bool =
    match decl with
    | Instance { argument=argument'; _ } ->
       overlapping_instances argument argument'
    | _ ->
       false
  in
  if List.exists pred instances then
    generic_error OverlappingInstances
  else
    ()

let rec check_instance_orphan_rules (env: env) (mod_id: mod_id) (typeclass_mod_id: mod_id) (ty: ty): unit =
  let tc_local: bool = equal_mod_id mod_id typeclass_mod_id
  and ty_local: bool = is_type_local env mod_id ty
  in
  if (not tc_local) && (not ty_local) then
    generic_error BrokenOrphanRule
  else
    ()

and is_type_local (env: env) (mod_id: mod_id) (ty: ty): bool =
  match ty with
  | Unit ->
     true
  | Boolean ->
     true
  | Integer _ ->
     true
  | SingleFloat ->
     true
  | DoubleFloat ->
     true
  | NamedType (name, _, _) ->
     let type_mod_id: mod_id = decl_mod_id (get_decl_by_name_or_die env (qident_to_sident name)) in
     equal_mod_id mod_id type_mod_id
  | RegionTy _ ->
     (* Not applicable. *)
     false
  | ReadRef _ ->
     true
  | WriteRef _ ->
     true
  | Span _ ->
     true
  | SpanMut _ ->
     true
  | TyVar _ ->
     (* Not applicable. *)
     false
  | Address _ ->
     true
  | Pointer _ ->
     true
  | FnPtr _ ->
     true
  | MonoTy _ ->
     (* Not applicable. *)
     false

let check_disjoint_typarams (name: identifier) (typarams: typarams): unit =
  match get_typaram typarams name with
  | Some _ ->
     declaration_error (InstanceShadowsTypeclassParam name)
  | None ->
     (* No collision *)
     ()
