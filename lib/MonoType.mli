(** The monomorphic type system. *)
open Identifier
open Type

(** The ID of a type monomorph. *)
type mono_type_id

(** A monomorphic, or concrete, type. *)
type mono_ty =
  | MonoUnit
  | MonoBoolean
  | MonoInteger of signedness * integer_width
  | MonoSingleFloat
  | MonoDoubleFloat
  (** A monomorphic instance of a generic type, identified by the name of the type, its monomorphic arguments, and a monomorph ID. *)
  | NamedType of qident * mono_ty list * mono_type_id
  | MonoArray of mono_ty
  | MonoReadRef of mono_ty
  | MoonWriteRef of mono_ty
  | MonoRawPointer of mono_ty
