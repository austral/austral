(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The combining pass takes the concrete representation of module interfaces
    and module bodies, and combines them into a single object for simplicity. *)
open Cst
open Stages.Combined
open Env

(** Combine a concrete module interface and a concrete module body into a
    combined module. *)
val combine : env -> concrete_module_interface -> concrete_module_body -> combined_module

(** Make a combined module out of a module body, with all declarations being public. *)
val body_as_combined : env -> concrete_module_body -> combined_module
