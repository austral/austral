(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open TypeBindings
open Tast

(** Replace the type variables in a typed expression. *)
val replace_tyvars_expr : type_bindings -> texpr -> texpr

(** Replace the type variables in a typed statement. *)
val replace_tyvars_stmt : type_bindings -> tstmt -> tstmt
