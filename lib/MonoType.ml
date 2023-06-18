(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The monomorphic type system. *)
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
  | MonoRegionTy of region
  | MonoReadRef of mono_ty * mono_ty
  | MonoWriteRef of mono_ty * mono_ty
  | MonoSpan of mono_ty * mono_ty
  | MonoSpanMut of mono_ty * mono_ty
  | MonoAddress of mono_ty
  | MonoPointer of mono_ty
  | MonoFnPtr of mono_ty list * mono_ty
  | MonoRegionTyVar of identifier * qident
[@@deriving (eq, show)]

(** A monomorphic record slot. *)
type mono_slot = MonoSlot of identifier * mono_ty

(** A monomorphic union case. *)
type mono_case = MonoCase of identifier * mono_slot list
