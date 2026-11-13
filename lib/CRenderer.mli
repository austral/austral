(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open CRepr

type indentation

type line

(** The text of the whole program. The parts are separated by a newline. *)
val render_program : c_program -> string

val render_expr : c_expr -> string
