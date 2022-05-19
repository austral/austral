(** This module implements Austral's lifetime analysis pass. *)
open Identifier
open Type
open LifetimeTables
open Tast
open Ptast

(** Given a list of value parameters, return a table of appearances where linear
    parameters are stored in the table as variables. *)
val appearances_from_params : value_parameter list -> appear_tbl

(** Given an expression, extract all the variables in the expression that are
    mentioned in the table of appearances, along with the kind of appearance
    they have. *)
val extract_variables : appear_tbl -> texpr -> (identifier * appear_kind) list

(** Given a statement, do a depth-first pass and record variable appearances in the table. *)
val record_appearances : appear_tbl -> pstmt -> appear_tbl
