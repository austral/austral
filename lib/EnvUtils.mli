(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Environment utilities. *)
open Identifier
open Id
open EnvTypes
open Type

(** Return the ID of a declaration. *)
val decl_id : decl -> decl_id

(** Return the ID of the module a decl belongs to. *)
val decl_mod_id : decl -> mod_id

(** Return the name of a declaration. *)
val decl_name : decl -> identifier option

(** Return whether a declaration is importable by a foreign module. *)
val is_importable: decl -> bool

val union_case_to_typed_case : decl -> typed_case
