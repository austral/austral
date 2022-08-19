(** This module defines the type_parameter type. *)
open Identifier
open Type

(** Represents type parameters. *)
type type_parameter
[@@deriving (show, sexp)]

val make_typaram : identifier * universe * qident * sident list -> type_parameter

val typaram_name : type_parameter -> identifier

val typaram_universe : type_parameter -> universe

val typaram_constraints : type_parameter -> sident list

val typaram_to_tyvar : type_parameter -> type_var
