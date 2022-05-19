(** This module implements Austral's lifetime analysis pass. *)
open Type
open Appearances

(** Given a list of value parameters, return a table of appearances where linear
    parameters are stored in the table as variables. *)
val appearances_from_params : value_parameter list -> appear_tbl
