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

module Errors = struct
  let broken_orphan_rule () =
    austral_raise DeclarationError [
      Text "Orphan rule broken: typeclass and type are both foreign."
    ]

  let duplicate_tyvars () =
    austral_raise DeclarationError [
      Text "Generic instances can only use the type parameters once in the argument."
    ]

  let leftovers () =
    austral_raise DeclarationError [
      Text "The number of type parameters in the instance declaration must be the same as the number of type variables applied to the argument."
    ]

  let lone_tyvar () =
    austral_raise DeclarationError [
      Text "Typeclass arguments cannot be lone type parameters."
    ]

  let not_a_tyvar ty =
    austral_raise DeclarationError [
      Text "The type ";
      Code (type_string ty);
      Text " is not a type parameter, but generic instances must use either all type parameters or none of them in the argument.";
      Break;
      Text "Consider adding another generic parameter with universe ";
      Code (type_universe ty |> universe_string)
    ]

  let overlapping_instances () =
    austral_raise DeclarationError [
      Text "Instance overlap."
    ]

  let region_arg () =
    austral_raise DeclarationError [
      Text "Typeclass arguments cannot be regions."
    ]

  let shadowing_typeclass_param name =
    austral_raise DeclarationError [
      Text "The type parameters in a generic instance declaration cannot have the same name as the type parameter in the corresponding typeclass. The colliding name is ";
      Code (ident_string name);
      Text ".";
      Break;
      Text "This is, regrettably, a load-bearing hack for https://github.com/austral/austral/issues/244. Fixing this properly would require rewriting large parts of the frontend."
    ]

  let wrong_universe ~ty ~expected ~actual =
    austral_raise TypeError [
      Text "Cannot define an instance with argument ";
      Code (type_string ty);
      Text " because it needs to be ";
      Code (universe_string expected);
      Text " but it is ";
      Code (universe_string actual)
    ]
end

let check_instance_argument_has_right_universe (universe: universe) (arg: ty): unit =
  if universe_compatible universe (type_universe arg) then
    ()
  else
    Errors.wrong_universe
      ~ty:arg
      ~expected:universe
      ~actual:(type_universe arg)

let check_instance_argument_has_right_shape (typarams: typarams) (arg: ty): unit =
  let is_tyvar (ty: ty): type_var =
    match ty with
    | TyVar tv ->
       tv
    | _ ->
       Errors.not_a_tyvar ty
  and all_distinct (tyvars: type_var list): unit =
    let names: identifier list = List.map (fun (TypeVariable (name, _, _, _)) -> name) tyvars in
    let set: IdentifierSet.t = IdentifierSet.of_list names in
    if List.length tyvars = IdentifierSet.cardinal set then
      ()
    else
      Errors.duplicate_tyvars ()
  (* Assert the number of type parameters *)
  and no_leftovers (n: int): unit =
    if (typarams_size typarams) = n then
      ()
    else
      Errors.leftovers ()
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
     Errors.region_arg ()
  | ReadRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | WriteRef (ty, r) ->
     let _ = all_distinct [is_tyvar ty; is_tyvar r] in
     let _ = no_leftovers 2 in
     ()
  | TyVar _ ->
     Errors.lone_tyvar ()
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
     err "Not allowed"

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
  | StaticArray _ ->
     (match b with
      | StaticArray _ -> true
      | _ -> false)
  | RegionTy _ ->
     err "Not allowed"
  | ReadRef _ ->
     (match b with
      | ReadRef _ -> true
      | _ -> false)
  | WriteRef _ ->
     (match b with
      | WriteRef _ -> true
      | _ -> false)
  | TyVar _ ->
     err "Not allowed"
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
     err "Not allowed"

let check_instance_locally_unique (instances: decl list) (argument: ty): unit =
  let pred (decl: decl): bool =
    match decl with
    | Instance { argument=argument'; _ } ->
       overlapping_instances argument argument'
    | _ ->
       false
  in
  if List.exists pred instances then
    Errors.overlapping_instances ()
  else
    ()

let rec check_instance_orphan_rules (env: env) (mod_id: mod_id) (typeclass_mod_id: mod_id) (ty: ty): unit =
  let tc_local: bool = equal_mod_id mod_id typeclass_mod_id
  and ty_local: bool = is_type_local env mod_id ty
  in
  if (not tc_local) && (not ty_local) then
    Errors.broken_orphan_rule ()
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
  | StaticArray _ ->
     true
  | RegionTy _ ->
     (* Not applicable. *)
     false
  | ReadRef _ ->
     true
  | WriteRef _ ->
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
     Errors.shadowing_typeclass_param name    
  | None ->
     (* No collision *)
     ()