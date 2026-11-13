(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open CRepr

type indentation

type line

val render_unit : c_unit -> string

val render_expr : c_expr -> string
