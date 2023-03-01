(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Cst

val parse_module_int : string -> string -> concrete_module_interface
val parse_module_body : string -> string -> concrete_module_body
val parse_stmt : string -> cstmt
val parse_expr : string -> cexpr
