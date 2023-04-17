(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open IdentifierSet
open Type
open TypeBindings
open TypeParameter
open TypeParameters
open Tast
open TastUtil
open Env
open EnvTypes
open EnvExtras
open Reporter
open Error

module Errors = struct
  let type_mismatch a b =
    austral_raise TypeError [
      Text "Expected a value of type ";
      Type a;
      Text ", but got a value of type ";
      Type b;
      Text "."
    ]

  let unsatisfied_constraint (tv: type_var) (ty: ty) (constraint_typeclass_name: sident) =
    let (TypeVariable (tv_name, _, _, _)) = tv in
    austral_raise TypeError [
        Text "Type constraint not satisfied: the type ";
        Type ty;
        Text " does not implement the typeclass ";
        Code (ident_string (sident_name constraint_typeclass_name));
        Text ".";
        Break;
        Text "This constraint is for the type variable ";
        Code (ident_string tv_name);
        Text "."
    ]
end

type ctx = env * module_name

let ctx_env (env, _) = env

let ctx_mn (_, mn) = mn

let rec match_type (ctx: ctx) (a: ty) (b: ty): type_bindings =
  pt ("A", a);
  pt ("B", b);
  match a with
  | Unit ->
     (match b with
      | Unit ->
         empty_bindings
      | _ ->
         Errors.type_mismatch a b)
  | Boolean ->
     (match b with
      | Boolean ->
         empty_bindings
      | _ ->
         Errors.type_mismatch a b)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         if (s = s') && (w = w') then
           empty_bindings
         else
           Errors.type_mismatch a b
      | _ ->
         Errors.type_mismatch a b)
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         empty_bindings
      | _ ->
         Errors.type_mismatch a b)
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         empty_bindings
      | _ ->
         Errors.type_mismatch a b)
  | NamedType (n, args, _) ->
     (match b with
      | NamedType (n', args', _) ->
         (* We ignore the universe, since the type system is nominal. *)
         if n = n' then
           match_type_list ctx args args'
         else
           Errors.type_mismatch a b
      | _ ->
         Errors.type_mismatch a b)
  | StaticArray t ->
     (match b with
      | StaticArray t' ->
         match_type ctx t t'
      | _ ->
         Errors.type_mismatch a b)
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         if r = r' then
           empty_bindings
         else
           Errors.type_mismatch a b
      | _ ->
         Errors.type_mismatch a b)
  | ReadRef (t, r) ->
     (match b with
      | ReadRef (t', r') ->
         let bindings = match_type ctx t t' in
         let bindings' = match_type ctx r r' in
         merge_bindings bindings bindings'
      | _ ->
         Errors.type_mismatch a b)
  | WriteRef (t, r) ->
     (match b with
      | WriteRef (t', r') ->
         let bindings = match_type ctx t t' in
         let bindings' = match_type ctx r r' in
         merge_bindings bindings bindings'
      | _ ->
         Errors.type_mismatch a b)
  | TyVar tyvar ->
     match_type_var ctx tyvar b
  | Address t ->
     (match b with
      | Address t' ->
         match_type ctx t t'
      | _ ->
         Errors.type_mismatch a b)
  | Pointer t ->
     (match b with
      | Pointer t' ->
         match_type ctx t t'
      | _ ->
         Errors.type_mismatch a b)
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') ->
         match_type_list ctx (rt :: args) (rt' :: args')
      | _ ->
         Errors.type_mismatch a b)
  | MonoTy _ ->
     err "match_type called with MonoTy argument"

and match_type_var (ctx: ctx) (tv: type_var) (ty: ty): type_bindings =
  let (TypeVariable (name, universe, from, constraints)) = tv in
  (* Check if the argument type is a variable. *)
  match ty with
  | (TyVar (TypeVariable (i', u', from', constraints'))) ->
     (* When the argument type is a type variable, check if the variables have
        the same name and provenance. *)
     if (equal_identifier name i') && (universe = u') && (equal_qident from from') then
       (* If so, do nothing: we don't want circular bindings *)
       empty_bindings
     else
       (* Check that the tyvar implements the type variable's constraints, if any. *)
       let _ = check_tyvar_implements_constraints constraints constraints' in
       (* Otherwise, add a new binding.

          The idea here is: suppose we have a function `f` that accepts an
          argument of a generic type `T`. Now we're type-checking a function `g`
          that accepts an argument `x` of generic type `U`. If we encounter a
          function call `f(x)`, then the parameter type is `T` and the argument
          type is `U`, so we create a binding `T -> U`.  *)
       add_binding empty_bindings (tyvar_to_typaram tv) ty
  | _ ->
     (* Check that the type implements the type variable's constraints, if any. *)
     check_type_implements_constraints ctx tv ty constraints;
     (* If the constraints are satisfied, add a straightforward binding. *)
     add_binding empty_bindings (tyvar_to_typaram tv) ty

and check_tyvar_implements_constraints (param: sident list) (arg: sident list): bool =
  (* Check that the param is a subset of the param. *)
  let param: SIdentSet.t = SIdentSet.of_list param
  and arg: SIdentSet.t = SIdentSet.of_list arg
  in
  SIdentSet.subset param arg

and check_type_implements_constraints (ctx: ctx) (tv: type_var) (ty: ty) (constraints: sident list): unit =
  with_frame "Check type implements constraints"
    (fun _ ->
      pt ("Type", ty);
      ps ("Constraints", "[" ^ (String.concat ", " (List.map show_sident constraints)) ^ "]");
      (* If there are no constraints, do nothing. *)
      if constraints = [] then
        ()
      else
        (* If there are constraints, make them into a set. *)
        let constraints: SIdentSet.t = SIdentSet.of_list constraints in
        (* Try to find an instance for this type for each of the constraints. *)
        List.iter (try_constraint ctx tv ty) (List.of_seq (SIdentSet.to_seq constraints)))

and try_constraint (ctx: ctx) (tv: type_var) (ty: ty) (typeclass_name: sident): unit =
  (* Find the typeclass. *)
  match get_decl_by_name (ctx_env ctx) typeclass_name with
  | Some (TypeClass { id; _ }) ->
     (* Try to find an instance for the given typeclass name that implements this type. *)
     (match get_instance (ctx_env ctx) (ctx_mn ctx) ty id with
      | Some _ ->
         ()
      | None ->
         Errors.unsatisfied_constraint tv ty typeclass_name)
  | Some _ ->
     internal_err "Type parameter constraint refers to a declaration which is not a typeclass."
  | None ->
     internal_err "No typeclass with the given name. This should have been validated in the extraction pass."

and get_instance (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: decl_id): (decl * type_bindings) option =
  with_frame "Typeclass Resolution"
    (fun _ ->
      ps ("In module", (mod_name_string source_module_name));
      ps ("Typeclass", (get_decl_name_or_die env typeclass));
      pt ("Dispatch type", dispatch_ty);
      let mod_id: mod_id =
        let (ModRec { id; _ }) = Option.get (get_module_by_name env source_module_name) in
        id
      in
      let pred (decl: decl): (decl * type_bindings) option =
        match decl with
        | Instance { typeclass_id; argument; _ } ->
           if equal_decl_id typeclass_id typeclass then
             let _ = pt ("Trying instance with argument", argument) in
             try
               let bindings = match_type (env, source_module_name) argument dispatch_ty in
               ps ("Resolution", "Success");
               Some (decl, bindings)
             with
               Austral_error _ ->
               (* Does not match, just skip to the next instance, *)
               ps ("Resolution", "Failure");
               None
           else
             None
        | _ ->
           None
      in
      let filtered: (decl * type_bindings) list =
        with_frame "Filtering instances"
          (fun _ -> List.filter_map pred (visible_instances env mod_id))
      in
      match filtered with
      | [a] ->
         Some a
      | _::_ ->
         internal_err "Multiple instances satisfy this call. The compiler should validate that there are no overlapping instances."
      | [] ->
         None)

and match_type_list ctx tys tys' =
  let bs = List.map2 (match_type ctx) tys tys' in
  List.fold_left merge_bindings empty_bindings bs

let match_typarams (ctx: ctx) (typarams: typarams) (args: ty list): type_bindings =
  let typarams' = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams) in
  match_type_list ctx typarams' args

let parse_bigint (s: string): Z.t =
  try
    Z.of_string s
  with
    | Invalid_argument _ ->
      internal_err ("Failed to parse '" ^ s ^ "' as a big int. This is a bug in the parser.")

let match_int_type_with_value (signedness: signedness) (width: integer_width) (value: string): type_bindings =
   let _ = (signedness, width, value) in
   (* Parse the constant as a big integer. *)
   let i: Z.t = parse_bigint value in
   let _ = i in
   empty_bindings

let match_type_with_value (ctx: ctx) (ty: ty) (expr: texpr): type_bindings =
  (* Like type_match, but gentler with integer constants. *)
  match ty with
  | Integer (signedness, width) ->
     (match expr with
      | TIntConstant value ->
         match_int_type_with_value signedness width value
      | _ ->
         match_type ctx ty (get_type expr))
  | SingleFloat ->
     (match expr with
      | TFloatConstant _ ->
         empty_bindings
      | _ ->
         match_type ctx ty (get_type expr))
  | DoubleFloat ->
     (match expr with
      | TFloatConstant _ ->
         empty_bindings
      | _ ->
         match_type ctx ty (get_type expr))
  | _ ->
     match_type ctx ty (get_type expr)