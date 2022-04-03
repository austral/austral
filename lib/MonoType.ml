(** An alternative implementation of the monomorphic type system. *)
open Id
open Identifier
open Type
open Region

(** A monomorphic type. *)
type mono_ty =
  | MonoUnit
  | MonoBoolean
  | MonoInteger of signedness * integer_width
  | MonoSingleFloat
  | MonoDoubleFloat
  | MonoNamedType of mono_id
  | MonoArray of mono_ty * region
  | MonoRegionTy of region
  | MonoReadRef of mono_ty * mono_ty
  | MonoWriteRef of mono_ty * mono_ty
  | MonoRawPointer of mono_ty
[@@deriving eq]

(** A monomorphic record slot. *)
type mono_slot = MonoSlot of identifier * mono_ty

(** A monomorphic union case. *)
type mono_case = MonoCase of identifier * mono_slot list
