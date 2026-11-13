(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The type checker. *)
open Env
open Stages.Linked
open Stages.Tast

(** The context for statement checking. *)
type stmt_ctx

val augment_module : env -> linked_module -> typed_module
