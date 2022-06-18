open Identifier
open Type
open TypeBindings
open TypeParameters
open Tast
open Error

let type_mismatch _ a b =
  austral_raise TypeError
    [
      Text "Expected a value of type";
      Code (type_string a);
      Text ", but got a value of type";
      Code (type_string b);
    ]

let rec match_type a b =
  match a with
  | Unit ->
     (match b with
      | Unit ->
         empty_bindings
      | _ ->
         type_mismatch "Expected Unit, but got another type." a b)
  | Boolean ->
     (match b with
      | Boolean ->
         empty_bindings
      | _ ->
         type_mismatch "Expected Boolean, but got another type." a b)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         if (s = s') && (w = w') then
           empty_bindings
         else
           type_mismatch "Integer types don't match" a b
      | _ ->
         type_mismatch "Expected an integer, but got another type." a b)
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         empty_bindings
      | _ ->
         type_mismatch "Expected SingleFloat, but got another type." a b)
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         empty_bindings
      | _ ->
         type_mismatch "Expected DoubleFloat, but got another type." a b)
  | NamedType (n, args, _) ->
     (match b with
      | NamedType (n', args', _) ->
         (* We ignore the universe, since the type system is nominal. *)
         if n = n' then
           match_type_list args args'
         else
           type_mismatch "Type mismatch" a b
      | _ ->
         type_mismatch "Expected a named type, but got something else." a b)
  | StaticArray t ->
     (match b with
      | StaticArray t' ->
         match_type t t'
      | _ ->
         type_mismatch "Expected an array, but got another type." a b)
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         if r = r' then
           empty_bindings
         else
           type_mismatch "Region type mismatch" a b
      | _ ->
         type_mismatch "Expected a region, but got another type." a b)
  | ReadRef (t, r) ->
     (match b with
      | ReadRef (t', r') ->
         let bindings = match_type t t' in
         let bindings' = match_type r r' in
         merge_bindings bindings bindings'
      | _ ->
         type_mismatch "Expected a read reference, but got another type." a b)
  | WriteRef (t, r) ->
     (match b with
      | WriteRef (t', r') ->
         let bindings = match_type t t' in
         let bindings' = match_type r r' in
         merge_bindings bindings bindings'
      | _ ->
         type_mismatch "Expected a write reference, but got another type." a b)
  | TyVar tyvar ->
     match_type_var tyvar b
  | Address t ->
     (match b with
      | Address t' ->
         match_type t t'
      | _ ->
        type_mismatch "Expected an Address, but got another type." a b)
  | Pointer t ->
     (match b with
      | Pointer t' ->
         match_type t t'
      | _ ->
        type_mismatch "Expected a Pointer, but got another type." a b)
  | MonoTy _ ->
    err "Not applicable"

and match_type_var (TypeVariable (name, universe, from)) ty =
  (* Check if the argument type is a variable. *)
  match ty with
  | (TyVar (TypeVariable (i', u', from'))) ->
     (* When the argument type is a type variable, check if the variables have
        the same name and provenance. *)
     if (equal_identifier name i') && (universe = u') && (equal_qident from from') then
       (* If so, do nothing: we don't want circular bindings *)
       empty_bindings
     else
       (* Otherwise, add a new binding.

          The idea here is: suppose we have a function `f` that accepts an
          argument of a generic type `T`. Now we're type-checking a function `g`
          that accepts an argument `x` of generic type `U`. If we encounter a
          function call `f(x)`, then the parameter type is `T` and the argument
          type is `U`, so we create a binding `T -> U`.  *)
       add_binding empty_bindings name from ty
  | _ ->
     (* The argument type is not a type variable. Add a straightforward
        binding. *)
     add_binding empty_bindings name from ty

and match_type_list tys tys' =
  let bs = List.map2 match_type tys tys' in
  List.fold_left merge_bindings empty_bindings bs

let match_typarams (typarams: typarams) (args: ty list): type_bindings =
  let typarams' = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams) in
  match_type_list typarams' args

let match_type_with_value (ty: ty) (expr: texpr): type_bindings =
  (* Like type_match, but gentler with integer constants. *)
  match ty with
  | Integer _ ->
     (match expr with
      | TIntConstant _ ->
         (* TODO: check sign against signedness *)
         (* TODO: check value against width *)
         empty_bindings
      | _ ->
         match_type ty (get_type expr))
  | _ ->
     match_type ty (get_type expr)
