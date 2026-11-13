(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The linearity checker. *)
open Stages.Tast

(** The loop depth represents, for a particular piece of code, how many loops
    into the function that piece of code is. *)
type loop_depth = int

(** The consumption state of a variable. *)
type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed

(** The state table maps linear variables to the loop depth at the point where
    they are defined and their consumption state. *)
type state_tbl

(** Represents the number of times a given variable appears in an expression in different ways. *)
type appearances

(** Check the linearity of all functions and methods in a module. *)
val check_module_linearity : typed_module -> unit
