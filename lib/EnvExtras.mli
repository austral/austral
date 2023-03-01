(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Useful functions not part of the core environment API. *)
open Id
open Identifier
open Env
open EnvTypes

val get_module_name_or_die : env -> mod_id -> module_name

val get_decl_by_name_or_die : env -> sident -> decl

val get_decl_sident_or_die : env -> decl_id -> sident

val get_decl_name_or_die : env -> decl_id -> string

val get_method_id_or_die : env -> decl_id -> qident -> decl_id
