(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open Region

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse
[@@deriving (eq, show, sexp)]

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64
  | WidthByteSize
  | WidthIndex
[@@deriving (eq, show, sexp)]

type signedness =
  | Unsigned
  | Signed
[@@deriving (eq, show, sexp)]

type type_var = TypeVariable of identifier * universe * qident * sident list
[@@deriving (eq, show, sexp)]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | StaticArray of ty
  | RegionTy of region
  | ReadRef of ty * ty
  | WriteRef of ty * ty
  | Span of ty * ty
  | SpanMut of ty * ty
  | TyVar of type_var
  | Address of ty
  | Pointer of ty
  | FnPtr of ty list * ty
  | MonoTy of mono_id
  (** Special case, see the `mono_to_ty` function. We need this to be able to do
     monomorph instantiation, but this doesn't correspond to anything in the
     code. *)
[@@deriving (show, sexp)]

type typed_slot = TypedSlot of identifier * ty
[@@deriving sexp]

type typed_case = TypedCase of identifier * typed_slot list
[@@deriving sexp]

type value_parameter = ValueParameter of identifier * ty
[@@deriving (show, sexp)]

val universe_string : universe -> string

(* A string representation of a type, for showing to the user. *)
val type_string : ty -> string

(* The type of array sizes and indices *)
val index_type : ty

val string_type : ty

val equal_ty : ty -> ty -> bool
