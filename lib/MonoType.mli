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
  | MonoNamedType of qident * mono_type_id
  (** A monomorphic instance of a generic type, identified by the name of the type, its monomorphic arguments, and a monomorph ID. *)
  | MonoArray of mono_ty
  | MonoReadRef of mono_ty
  | MonoWriteRef of mono_ty
  | MonoRawPointer of mono_ty

(** The table of type monomorphs associates a generic type's name and list of
   monomorphic type arguments to its type monomorph ID. *)
type mono_type_tbl
