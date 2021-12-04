open Type
open TypeVarSet
open Region

(* Get the universe this type belongs to *)
val type_universe : ty -> universe

(* Whether the type is a built-in integer or floating point type. *)
val is_numeric : ty -> bool

(* Whether the type is comparable (unit, bool, int, float) *)
val is_comparable : ty -> bool

(* Return the set of type variables in a type expression, *)
val type_variables : ty -> TypeVarSet.t

val region_map_from_typarams : type_parameter list -> region_map

val is_concrete : ty -> bool
