open Type
open TypeVarSet
open Region
open Error

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
  | Address _ -> FreeUniverse
  | MonoTy _ -> err "Not applicable"

let is_numeric = function
  | Unit -> false
  | Boolean -> false
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | Array _ -> false
  | RegionTy _ -> false
  | ReadRef _ -> false
  | WriteRef _ -> false
  | TyVar _ -> false
  | Address _ -> false
  | MonoTy _ -> err "Not applicable"

let is_comparable = function
  | Unit -> true
  | Boolean -> true
  | Integer _ -> true
  | SingleFloat -> true
  | DoubleFloat -> true
  | NamedType _ -> false
  | Array _ -> false
  | RegionTy _ -> false
  | ReadRef _ -> false
  | WriteRef _ -> false
  | TyVar _ -> false
  | Address _ -> true
  | MonoTy _ -> err "Not applicable"

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
  | Array (ty, _) ->
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
  | MonoTy _ ->
    TypeVarSet.empty

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
  | Address _ ->
    true
  | MonoTy _ ->
    true
