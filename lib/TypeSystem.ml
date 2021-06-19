open Type
open TypeVarSet
open Error

let type_universe = function
    Unit -> FreeUniverse
  | Boolean -> FreeUniverse
  | Integer _ -> FreeUniverse
  | SingleFloat -> FreeUniverse
  | DoubleFloat -> FreeUniverse
  | NamedType (_, _, u) -> u
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

let is_built_in_type = function
  | "Unit" ->
     Some Unit
  | "Boolean" ->
     Some Boolean
  | "Natural_8" ->
     Some (Integer (Unsigned, Width8))
  | "Natural_16" ->
     Some (Integer (Unsigned, Width16))
  | "Natural_32" ->
     Some (Integer (Unsigned, Width32))
  | "Natural_64" ->
     Some (Integer (Unsigned, Width64))
  | "Integer_8" ->
     Some (Integer (Signed, Width8))
  | "Integer_16" ->
     Some (Integer (Signed, Width16))
  | "Integer_32" ->
     Some (Integer (Signed, Width32))
  | "Integer_64" ->
     Some (Integer (Signed, Width64))
  | "SingleFloat" ->
     Some SingleFloat
  | "DoubleFloat" ->
     Some DoubleFloat
  | _ ->
     None

let rec effective_universe typarams declared_universe args =
  (* Algorithm:

     1. If the declared universe is Free then none of the type parameters can be Linear or Type.

     2. If the declared universe is Linear, then no type parameter can change this. Therefore, the effective
        universe is Linear.

     3. If the declared universe is Region, that's an internal error: this function should not be called with a Region type.

     4. Finally, if the universe is Type:
         4.1. First, assert that there is at least one type parameter. This should have been checked by the
              compiler when declarations are processed.

         4.2. If any argument's universe is Linear, the effective universe is Linear.

         4.3. Otherwise, if any argument's universe is Type, the effective universe is Type. The ordering
              here is because Linear is stronger (i.e. lower in the poset of universes) than Type.

         4.4. Otherwise, the effective universe is Free.
   *)
  match declared_universe with
  | FreeUniverse ->
     if all_arguments_are_free args then
       FreeUniverse
     else
       err "Free type called with non-free argument"
  | LinearUniverse ->
     LinearUniverse
  | RegionUniverse ->
     err "effective_universe called with a region type"
  | TypeUniverse ->
     assert ((List.length typarams) > 0);
     if any_arg_is_linear args then
       LinearUniverse
     else
       if any_arg_is_type args then
         TypeUniverse
       else
         FreeUniverse

and all_arguments_are_free (args: ty list): bool =
  let is_compatible_with_free = function
  | FreeUniverse ->
     true
  | RegionUniverse ->
     true
  | _ ->
     false
  in
  List.for_all is_compatible_with_free (List.map type_universe args)

and any_arg_is_linear (args: ty list) =
  let is_linear = function
    | LinearUniverse -> true
    | _ -> false
  in
  List.exists is_linear (List.map type_universe args)

and any_arg_is_type (args: ty list) =
  let is_type = function
    | TypeUniverse -> true
    | _ -> false
  in
  List.exists is_type (List.map type_universe args)
