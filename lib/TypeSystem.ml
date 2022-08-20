open Type
open TypeVarSet
open TypeParameters
open Region
open Error

let type_universe = function
    Unit -> FreeUniverse
  | Boolean -> FreeUniverse
  | Integer _ -> FreeUniverse
  | SingleFloat -> FreeUniverse
  | DoubleFloat -> FreeUniverse
  | NamedType (_, _, u) -> u
  | StaticArray _ -> FreeUniverse
  | RegionTy _ -> RegionUniverse
  | ReadRef _ -> FreeUniverse
  | WriteRef _ -> FreeUniverse
  | TyVar (TypeVariable (_, u, _, _)) -> u
  | Address _ -> FreeUniverse
  | Pointer _ -> FreeUniverse
  | FnPtr _ -> FreeUniverse
  | MonoTy _ -> FreeUniverse (*err "type_universe called with MonoTy argument"*)

let is_numeric = function
  | Unit -> false
  | Boolean -> false
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | StaticArray _ -> false
  | RegionTy _ -> false
  | ReadRef _ -> false
  | WriteRef _ -> false
  | TyVar _ -> false
  | Address _ -> false
  | Pointer _ -> false
  | FnPtr _ -> false
  | MonoTy _ -> err "is_numeric called with MonoTy argument"

let is_comparable = function
  | Unit -> true
  | Boolean -> true
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | StaticArray _ -> false
  | RegionTy _ -> false
  | ReadRef _ -> false
  | WriteRef _ -> false
  | TyVar _ -> false
  | Address _ -> true
  | Pointer _ -> true
  | FnPtr _ -> true
  | MonoTy _ -> err "is_comparable called with MonoTy argument"

let rec type_variables = function
  | Unit ->
     TypeVarSet.empty
  | Boolean ->
     TypeVarSet.empty
  | Integer _ ->
     TypeVarSet.empty
  | SingleFloat ->
     TypeVarSet.empty
  | DoubleFloat ->
     TypeVarSet.empty
  | NamedType (_, a, _) ->
     List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables a)
  | StaticArray ty ->
     type_variables ty
  | RegionTy _ ->
     TypeVarSet.empty
  | ReadRef (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables r)
  | WriteRef (ty, r) ->
     TypeVarSet.union (type_variables ty) (type_variables r)
  | TyVar v ->
     TypeVarSet.singleton v
  | Address ty ->
     type_variables ty
  | Pointer ty ->
     type_variables ty
  | FnPtr (args, rt) ->
     let args' = List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables args)
     and rt' = type_variables rt
     in
     TypeVarSet.union args' rt'
  | MonoTy _ ->
    TypeVarSet.empty

let region_map_from_typarams (typarams: typarams): region_map =
  let _ = typarams in
  empty_region_map

(* TODO: why does this not work? *)
(*let region_map_from_typarams (typarams: typarams): region_map =
  let folder (rm: region_map) (typaram: type_parameter): region_map =
    let (TypeParameter (name, u, _)) = typaram in
    if u = RegionUniverse then
      add_region rm name (fresh_region name)
    else
      rm
  in
  List.fold_left folder empty_region_map (typarams_as_list typarams)
 *)
let rec is_concrete = function
  | Unit -> true
  | Boolean -> true
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | StaticArray ty ->
     is_concrete ty
  | RegionTy _ ->
     true
  | ReadRef (ty, _) ->
     is_concrete ty
  | WriteRef (ty, _) ->
     is_concrete ty
  | TyVar _ ->
     (* Individual type variables need not be instantiated. *)
     true
  | Address _ ->
     true
  | Pointer _ ->
     true
  | FnPtr _ ->
     true
  | MonoTy _ ->
     true

(** Given a list of types, return whether any of them are in the Linear
    universe. *)
and any_arg_is_linear (args: ty list) =
  let is_linear = function
    | LinearUniverse -> true
    | _ -> false
  in
  List.exists is_linear (List.map type_universe args)

(** Given a list of types, return whether any of them are in the Type
    universe. *)
and any_arg_is_type (args: ty list) =
  let is_type = function
    | TypeUniverse -> true
    | _ -> false
  in
  List.exists is_type (List.map type_universe args)
