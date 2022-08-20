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
  | MonoStaticArray of mono_ty
  | MonoRegionTy of region
  | MonoReadRef of mono_ty * mono_ty
  | MonoWriteRef of mono_ty * mono_ty
  | MonoAddress of mono_ty
  | MonoPointer of mono_ty
  | MonoFnPtr of mono_ty list * mono_ty
  | MonoRegionTyVar of identifier * typaram_source
[@@deriving (eq, show)]

(** A monomorphic record slot. *)
type mono_slot = MonoSlot of identifier * mono_ty

(** A monomorphic union case. *)
type mono_case = MonoCase of identifier * mono_slot list
