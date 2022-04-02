open Type
open Identifier

(** A stripped type specifier is the same as a type specifier, but the region
    and type variable cases have been removed.

    The reason for this is: by the time we get to the monomorphization stage, all
    type variables should have been replaced by their type arguments. Also,
    regions don't exist in the monomorphization phase.
*)
type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SArray of stripped_ty
  | SReadRef of stripped_ty
  | SWriteRef of stripped_ty
  | SRawPointer of stripped_ty

(** Strip a type specifier. *)
val strip_type : ty -> stripped_ty
