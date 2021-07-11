open Type
open TypeVarSet

let type_universe = function
    Unit -> FreeUniverse
  | Boolean -> FreeUniverse
  | Integer _ -> FreeUniverse
  | SingleFloat -> FreeUniverse
  | DoubleFloat -> FreeUniverse
  | NamedType (_, _, u) -> u
  | Array (_, _) -> FreeUniverse
  | RegionTy _ -> RegionUniverse
  | TyVar (TypeVariable (_, u)) -> u

let type_arguments = function
  | NamedType (_, args, _) -> args
  | _ -> []

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
  | _ -> false

let rec type_variables = function
  | NamedType (_, a, _) ->
     List.fold_left TypeVarSet.union TypeVarSet.empty (List.map type_variables a)
  | TyVar v -> TypeVarSet.singleton v
  | _ -> TypeVarSet.empty
