(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Env
open Stages.Tast

(** Given a typed module, extract the function and instance method bodies and
    store them in the environment. *)
val extract_bodies : env -> typed_module -> env
