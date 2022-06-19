open Id
open Identifier
open IdentifierSet
open Type
open TypeParameters
open TypeParser
open TypeSystem
open TypeBindings
open Env
open EnvTypes
open EnvExtras
open EnvUtils
open TypeMatch
open Reporter
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
    err "Instance overlaps."
  else
    ()

let rec check_instance_orphan_rules (env: env) (mod_id: mod_id) (typeclass_mod_id: mod_id) (ty: ty): unit =
  let tc_local: bool = equal_mod_id mod_id typeclass_mod_id
  and ty_local: bool = is_type_local env mod_id ty
  in
  if (not tc_local) && (not ty_local) then
    err "Orphan rule broken: typeclass and type are both foreign."
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
  | MonoTy _ ->
     (* Not applicable. *)
     false

let get_instance (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: decl_id): decl * type_bindings =
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
               let bindings = match_type argument dispatch_ty in
               Some (decl, bindings)
             with
               Austral_error _ ->
               (* Does not match, just skip to the next instance, *)
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
         a
      | _::_ ->
         err "Multiple instances satisfy this call."
      | [] ->
         err (
             "Typeclass resolution failed. Typeclass: "
             ^ (get_decl_name_or_die env typeclass)
             ^ ". Dispatch type: "
             ^ (type_string dispatch_ty)))
