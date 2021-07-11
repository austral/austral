open Identifier
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

let region_map_from_typarams typarams =
  let filter (TypeParameter (n, u)) =
    if u = RegionUniverse then
      Some n
    else
      None
  in
  let rec map_from_list (rm: region_map) (names: identifier list): region_map =
    match names with
    | first::rest ->
       let rm' = add_region rm first (fresh_region first) in
       map_from_list rm' rest
    | [] ->
        rm
  in
  let names = List.filter_map filter typarams in
  map_from_list empty_region_map names
