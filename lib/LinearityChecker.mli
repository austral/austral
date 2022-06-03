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

(** Return all entries as a list. *)
val tbl_to_list : state_tbl -> (identifier * loop_depth * var_state) list

(** Represents the number of times a given variable appears in an expression in different ways. *)
type appearances

(** Count the appearances of a given variable in the expression. *)
val count : identifier -> texpr -> appearances

val linearity_check: value_parameter list -> tstmt -> unit
