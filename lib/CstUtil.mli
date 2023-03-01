(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Utilities for the concrete syntax tree. *)
open Identifier
open Cst
open Common

(** Used by the parser to easily create a module body from its components and a list of pragmas. *)
val make_module_body : module_name -> concrete_import_list list -> pragma list -> concrete_def list -> docstring -> concrete_module_body

val concrete_decl_name : concrete_decl -> identifier option

val def_name : concrete_def -> identifier option

val get_concrete_decl : concrete_module_interface -> identifier -> concrete_decl option

val get_concrete_def : concrete_module_body -> identifier -> concrete_def option

val has_instance_decl : concrete_module_interface -> identifier -> concrete_type_param list -> typespec -> bool

val get_instance_def : concrete_module_body -> identifier -> concrete_type_param list -> typespec -> concrete_instance option

val make_pragma : identifier -> concrete_arglist -> pragma

val mod_int_name : concrete_module_interface -> module_name

val mod_body_name : concrete_module_body -> module_name
