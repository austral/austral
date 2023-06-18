(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Type
open TypeParameter
open TypeSystem
open Sexplib
open Std
open Error

module Errors = struct
  let typaram_redefinition ~param ~was ~redef =
    austral_raise TypeError [
      Text "The type parameter ";
      Code (typaram_name param |> ident_string);
      Text " cannot be bound to both ";
      Type was;
      Text " and ";
      Type redef;
    ]

  let typaram_wrong_universe ~param ~ty ~expected ~actual =
    austral_raise TypeError [
      Text "The type parameter ";
      Code (typaram_name param |> ident_string);
      Text " must be ";
      Code (universe_string expected);
      Text " but ";
      Type ty;
      Text " is ";
      Code (universe_string actual)
    ]
end

let universe_compatible param arg =
  (* The check here is:

     1. If the parameter universe is Free, the argument universe must be Free.
     2. If the parameter universe is Linear, the argument universe must be Linear.
     3. If the parameter universe is Type, the argument universe must be any one of Free, Linear, or
        type.
     4. If the parameter universe is Region, the argument universe must be Region.
   *)
  match param with
  | FreeUniverse ->
     arg = FreeUniverse
  | LinearUniverse ->
     arg = LinearUniverse
  | TypeUniverse ->
     (arg = FreeUniverse) || (arg = LinearUniverse) || (arg = TypeUniverse)
  | RegionUniverse ->
     arg = RegionUniverse

(* I don't know why OCaml requires map elements to be totally ordered as opposed
   to just comparable for equality. Anyways, we get around this by basically
   serializing the type parameter into a string, which can be totally
   ordered. *)

let typaram_as_string (tp: type_parameter): string =
  let name: string = ident_string (typaram_name tp)
  and uni: string = show_universe (typaram_universe tp)
  and src: string = qident_debug_name (typaram_source tp)
  and cs: string = String.concat "," (List.map show_sident (typaram_constraints tp))
  in
  name ^ ";" ^ uni ^ ";" ^ src ^ ";" ^ cs

module BindingsMap =
  Map.Make(
      struct
        type t = type_parameter
        let compare (a: type_parameter) (b: type_parameter): int =
          compare (typaram_as_string a) (typaram_as_string b)
      end
    )

type type_bindings = TypeBindings of ty BindingsMap.t

let binding_count (TypeBindings m) =
  BindingsMap.cardinal m

let bindings_list (TypeBindings m) =
  (BindingsMap.bindings m)

let empty_bindings = TypeBindings BindingsMap.empty

let show_bindings (TypeBindings m) =
  let show_binding (tp, t) =
    (show_type_parameter tp) ^ " => " ^ (show_ty t)
  in
  "TypeBindings {" ^ (String.concat ", " (List.map show_binding (BindingsMap.bindings m))) ^ "}"

let get_binding (TypeBindings m) tp =
  BindingsMap.find_opt tp m

(* Add a binding to the map.

   If a binding with this name already exists, fail if the types are
   distinct. *)
let add_binding (TypeBindings m) (tp: type_parameter) (ty: ty): type_bindings =
  match BindingsMap.find_opt tp m with
  | Some ty' ->
     if equal_ty ty ty' then
       TypeBindings m
     else
       Errors.typaram_redefinition
         ~param:tp
         ~was:ty'
         ~redef:ty
  | None ->
     match ty with
     | MonoTy _ ->
       (* Special case! Allow `MonoTy`. See #399. *)
       TypeBindings (BindingsMap.add tp ty m)
     | _ ->
      if universe_compatible (typaram_universe tp) (type_universe ty) then
        TypeBindings (BindingsMap.add tp ty m)
      else
        Errors.typaram_wrong_universe
          ~param:tp
          ~ty
          ~expected:(typaram_universe tp)
          ~actual:(type_universe ty)

(* Add multiple bindings to a bindings map. *)
let rec add_bindings bs pairs =
  match pairs with
  | (tp, ty)::rest -> add_bindings (add_binding bs tp ty) rest
  | [] -> bs

let merge_bindings (TypeBindings a) (TypeBindings b) =
  let m = add_bindings empty_bindings (List.map (fun (tp, t) -> (tp, t)) (BindingsMap.bindings a)) in
  add_bindings m (List.map (fun (tp, t) -> (tp, t)) (BindingsMap.bindings b))

let rec replace_variables bindings ty =
  match ty with
  | Unit ->
     Unit
  | Boolean ->
     Boolean
  | Integer (s, w) ->
     Integer (s, w)
  | SingleFloat ->
     SingleFloat
  | DoubleFloat ->
     DoubleFloat
  | NamedType (n, a, u) ->
     let a' = List.map (replace_variables bindings) a in
     if u = TypeUniverse then
       let u' = if any_arg_is_linear a' then
                  LinearUniverse
                else
                  if any_arg_is_type a' then
                    TypeUniverse
                  else
                    FreeUniverse
       in
       NamedType (n, a', u')
     else
       NamedType (n, a', u)
  | TyVar tv ->
     (match get_binding bindings (tyvar_to_typaram tv) with
      | Some ty -> ty
      | None -> TyVar tv)
  | RegionTy r ->
     RegionTy r
  | ReadRef (ty, region) ->
     ReadRef (replace_variables bindings ty, replace_variables bindings region)
  | WriteRef (ty, region) ->
     WriteRef (replace_variables bindings ty, replace_variables bindings region)
  | Span (ty, region) ->
     Span (replace_variables bindings ty, replace_variables bindings region)
  | SpanMut (ty, region) ->
     SpanMut (replace_variables bindings ty, replace_variables bindings region)
  | Address ty ->
     Address (replace_variables bindings ty)
  | Pointer ty ->
     Pointer (replace_variables bindings ty)
  | FnPtr (args, rt) ->
     FnPtr (List.map (replace_variables bindings) args, replace_variables bindings rt)
  | MonoTy id ->
     MonoTy id

let rec bindings_from_list lst =
  match lst with
  | (tp, ty)::rest ->
     let bindings = empty_bindings in
     let bindings = add_binding bindings tp ty in
     merge_bindings bindings (bindings_from_list rest)
  | [] ->
   empty_bindings

let pp_type_bindings _ _ = ()

let show_type_bindings = show_bindings

type serializable_bindings = (type_parameter * ty) list
[@@deriving sexp]

let type_bindings_of_sexp (sexp: Sexp.t): type_bindings =
  bindings_from_list (serializable_bindings_of_sexp sexp)

let sexp_of_type_bindings (bs: type_bindings): Sexp.t =
  sexp_of_serializable_bindings (bindings_list bs)
