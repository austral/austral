(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** This module implements a check over the TAST that a function ends with
    either a return statement or a call to `Abort`. *)
open Stages.Combined

val check_ends_in_return : combined_module -> unit
