(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Entrypoint logic. *)
open Env
open Identifier

(** Given the env, and the name of the entrypoint function, generate the entrypoint code. *)
val entrypoint_code : env -> qident -> string
