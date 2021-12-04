open Type
open TypeVarSet
open Region

let type_universe = function
    Unit -> FreeUniverse
  | Boolean -> FreeUniverse
  | Integer _ -> FreeUniverse
  | SingleFloat -> FreeUniverse
  | DoubleFloat -> FreeUniverse
  | NamedType (_, _, u) -> u
  | Array (_, _) -> FreeUniverse
  | RegionTy _ -> RegionUniverse
  | ReadRef _ -> FreeUniverse
  | WriteRef _ -> FreeUniverse
  | TyVar (TypeVariable (_, u, _)) -> u
  | RawPointer _ -> FreeUniverse

let is_numeric = function
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | _ -> false

let is_comparable = function
  | Unit -> true
  | Boolean -> true
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | RawPointer _ -> true
  | _ -> false

let rec type_variables = function
  | NamedType (_, a, _) ->
     List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables a)
  | TyVar v -> TypeVarSet.singleton v
  | _ -> TypeVarSet.empty

let region_map_from_typarams _ =
  empty_region_map

let rec is_concrete = function
  | Unit -> true
  | Boolean -> true
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | Array (ty, _) ->
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
  | RawPointer _ ->
     true
