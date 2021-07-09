open Type
open TypeBindings

exception Type_match_error of string

let rec match_type a b =
  match (a, b) with
  | (Unit, Unit) -> empty_bindings
  | (Boolean, Boolean) -> empty_bindings
  | (Integer (s, w), Integer (s', w')) ->
     if (s = s') && (w = w') then
       empty_bindings
     else
       raise (Type_match_error ("Integer types don't match: expected " ^ (type_string a) ^ " but got " ^ (type_string b)))
  | (SingleFloat, SingleFloat) -> empty_bindings
  | (DoubleFloat, DoubleFloat) -> empty_bindings
  | (NamedType (n, a, _), NamedType (n', a', _)) ->
     (* We ignore the universe, since the type system
        is nominal. *)
     if n = n' then
       match_type_list a a'
     else
       raise (Type_match_error "Type mismatch")
  | (Array (t, r), Array (t', r')) ->
     let bindings = match_type t t' in
     if r = r' then
       bindings
     else
       raise (Type_match_error "Type mismatch: array region mismatch")
  | (TyVar (TypeVariable (i, u)), t) ->
     match_type_var i u t
  | _ ->
     raise (Type_match_error "Type mismatch")

and match_type_var name universe ty =
  (* Check if the argument type is a variable. *)
  match ty with
  | (TyVar (TypeVariable (i', u'))) ->
     (* When the argument type is a type variable, check if the variables have
        the same name. *)
     if (name = i') && (universe = u') then
       (* If the variables have the same name, return the empty map *)
       empty_bindings
     else
       (* Otherwise, add a new binding.

          The idea here is: suppose we have a function `f` that accepts an
          argument of a generic type `T`. Now we're type-checking a function `g`
          that accepts an argument `x` of generic type `U`. If we encounter a
          function call `f(x)`, then the parameter type is `T` and the argument
          type is `U`, so we create a binding `T -> U`.  *)
       add_binding empty_bindings name ty
  | _ ->
     (* The argument type is not a type variable. Add a straightforward
        binding. *)
     add_binding empty_bindings name ty

and match_type_list tys tys' =
  let bs = List.map2 match_type tys tys' in
  List.fold_left merge_bindings empty_bindings bs

let match_typarams typarams args =
  let typarams' = List.map (fun (TypeParameter (n, u)) -> TyVar (TypeVariable (n, u))) typarams in
  match_type_list typarams' args
