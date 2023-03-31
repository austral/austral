(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The linearity checker. *)
open Identifier
open Tast
open Type

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

(** The empty state table. *)
val empty_tbl : state_tbl

(** Find an entry in the state table. *)
val get_entry : state_tbl -> identifier -> (loop_depth * var_state) option

(** Add a new entry to the state table. Throws an error if an entry with that name already exists. All new variables start as unconsumed. *)
val add_entry : state_tbl -> identifier -> loop_depth -> state_tbl

(** Update the state of a variable in the table. Throws an error if there is no entry with this name. *)
val update_tbl : state_tbl -> identifier -> var_state -> state_tbl

(** Remove a row from the table. If the variable is not consumed, throws an error. *)
val remove_entry : state_tbl -> identifier -> state_tbl

(** Remove a set of variables from the table. If any of them are not consumed, throws an error. *)
val remove_entries : state_tbl -> identifier list -> state_tbl

(** When a variable defined outside a loop is consumed inside a loop, it is marked as pending. *)
val mark_pending : state_tbl -> identifier -> state_tbl

(** When a pending variable is assigned to, it is removed from the pending list. *)
val remove_pending : state_tbl -> identifier -> state_tbl

(** Return all entries as a list. *)
val tbl_to_list : state_tbl -> (identifier * loop_depth * var_state) list

(** Represents the number of times a given variable appears in an expression in different ways. *)
type appearances

(** Count the appearances of a given variable in the expression. *)
val count : identifier -> texpr -> appearances

(** Run the linearity checking algorithm given a function or method's value
    parameter list and body. *)
val linearity_check: value_parameter list -> tstmt -> unit

(** Given a list of value parameters, add the linearly-typed ones to the table. *)
val init_tbl : state_tbl -> value_parameter list -> state_tbl

(** Traverse a statement, applying the linearity checking rules. *)
val check_stmt : state_tbl -> loop_depth -> tstmt -> state_tbl

(** Given a state table, loop depth, and an expression, check the variables in
    the table against the expression, ensure they are used correctly, and update
    the table if needed. *)
val check_expr : state_tbl -> loop_depth -> texpr -> state_tbl

(** Given two tables, check that:

    1. Both have the same set of variables.

    2. The state of each variable is the same in both tables.

The first argument is the string to show in the error message describing what
kind of statement this is, with the article, so either `an if` or `a case`. *)
val tables_are_consistent : string -> state_tbl -> state_tbl -> unit

(** Like {!tables_are_consistent} but for a list of tables. If the list is empty
    or the singleton list, does nothing. *)
val table_list_is_consistent : state_tbl list -> unit

(** Check the linearity of all functions and methods in a module. *)
val check_module_linearity : typed_module -> unit
