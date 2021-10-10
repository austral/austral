open Identifier
open Type
open TypeBindings
open Tast
open Error

let type_mismatch _ a b =
  raise_type_mismatch_error (type_string a) (type_string b)

let rec match_type a b =
  match (a, b) with
  | (Unit, Unit) -> empty_bindings
  | (Boolean, Boolean) -> empty_bindings
  | (Integer (s, w), Integer (s', w')) ->
     if (s = s') && (w = w') then
       empty_bindings
     else
       type_mismatch "Integer types don't match" a b
  | (SingleFloat, SingleFloat) -> empty_bindings
  | (DoubleFloat, DoubleFloat) -> empty_bindings
  | (NamedType (n, args, _), NamedType (n', args', _)) ->
     (* We ignore the universe, since the type system
        is nominal. *)
     if n = n' then
       match_type_list args args'
     else
       type_mismatch "Type mismatch" a b
  | (Array (t, r), Array (t', r')) ->
     let bindings = match_type t t' in
     if r = r' then
       bindings
     else
       type_mismatch "Array type mismatch" a b
  | (RegionTy r, RegionTy r') ->
     if r = r' then
       empty_bindings
     else
       type_mismatch "Region type mismatch" a b
  | (ReadRef (t, r), ReadRef (t', r')) ->
     let bindings = match_type t t' in
     let bindings' = match_type r r' in
     merge_bindings bindings bindings'
  | (WriteRef (t, r), WriteRef (t', r')) ->
     let bindings = match_type t t' in
     let bindings' = match_type r r' in
     merge_bindings bindings bindings'
  | (TyVar (TypeVariable (i, u, from)), t) ->
     match_type_var i u from t
  | (t, TyVar (TypeVariable (i, u, from))) ->
     match_type_var i u from t
  | _ ->
     type_mismatch "Type mismatch" a b

and match_type_var name universe from ty =
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

let match_typarams typarams args =
  let typarams' = List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) typarams in
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
