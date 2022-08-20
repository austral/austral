open Id
open Type
open Identifier
open Region

(** A stripped type specifier is the same as a type specifier, but type variable
   case have been removed.

    The reason for this is: by the time we get to the monomorphization stage,
   all type variables should have been replaced by their type arguments.  *)
type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SStaticArray of stripped_ty
  | SRegionTy of region
  | SReadRef of stripped_ty * stripped_ty
  | SWriteRef of stripped_ty * stripped_ty
  | SAddress of stripped_ty
  | SPointer of stripped_ty
  | SFnPtr of stripped_ty list * stripped_ty
  | SMonoTy of mono_id
  | SRegionTyVar of identifier * typaram_source

(** Strip a type specifier. *)
val strip_type : ty -> stripped_ty
