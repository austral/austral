(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Id
open Type
open Env
open Stages.Mtast
open CRepr

val gen_decl_id : decl_id -> CIdent.t

val gen_mono_id : mono_id -> CIdent.t

(** The C name of a variable or a parameter. *)
val gen_ident : identifier -> CIdent.t

val fn_type : c_ty

(** The C type of an integer type. *)
val gen_integer_type : signedness -> integer_width -> c_ty

val gen_module : env -> mono_module -> c_unit
