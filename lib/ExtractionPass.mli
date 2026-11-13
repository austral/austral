(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages.SmallCombined
open Stages.Linked
open Env

(** Extract a combined module's declarations and put them in the environment,
    and returned a linked module. *)
val extract : env -> combined_module -> (env * linked_module)
