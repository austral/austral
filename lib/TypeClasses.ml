open Identifier
open IdentifierSet
open Type
open TypeParameters
open TypeParser
open TypeSystem
open Error

let check_instance_argument_has_right_universe (universe: universe) (arg: ty): unit =
  if universe_compatible universe (type_universe arg) then
    ()
  else
    err "While trying to define this instance, the instance's argument belongs to a universe that is not compatible with the typeclass's universe."

let check_instance_argument_has_right_shape (typarams: typarams) (arg: ty): unit =
  let is_tyvar (ty: ty): type_var =
    match ty with
    | TyVar tv ->
       tv
    | _ ->
       err "Not a tyvar"
  and all_distinct (tyvars: type_var list): unit =
    let names: identifier list = List.map (fun (TypeVariable (name, _, _)) -> name) tyvars in
    let set: IdentifierSet.t = IdentifierSet.of_list names in
    if List.length tyvars = IdentifierSet.cardinal set then
      ()
    else
      err "All tyvars must be distinct"
  (* Assert the number of type parameters *)
  and no_leftovers (n: int): unit =
    if (typarams_size typarams) = n then
      ()
    else
      err "The number of type parameters in the instance declaration must be the same as the number of type variables applied to the argument."
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
  | StaticArray ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | RegionTy _ ->
     err "Not a type variable."
  | ReadRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | WriteRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | TyVar _ ->
     err "Bad shape"
  | Address ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | Pointer ty ->
     let _ = is_tyvar ty in
     let _ = no_leftovers 1 in
     ()
  | MonoTy _ ->
     ()
