open Type
open TypeVarSet

(* Get the universe this type belongs to *)
val type_universe : ty -> universe

(* Get the arguments applied to this type constructor, if any. *)
val type_arguments : ty -> ty list

(* Whether the type is a built-in integer or floating point type. *)
val is_numeric : ty -> bool

(* Whether the type is comparable (unit, bool, int, float) *)
val is_comparable : ty -> bool

(* Return the set of type variables in a type expression, *)
val type_variables : ty -> TypeVarSet.t

val is_built_in_type : string -> ty option

(* Given a type's list of type parameters, its declared universe, and a list of
   supplied type arguments, evaluate the effective universe the type belongs to.

   Preconditions: the lists have the same length. *)
val effective_universe : type_parameter list -> universe -> ty list -> universe
