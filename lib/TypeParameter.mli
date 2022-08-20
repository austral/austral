(** This module defines the type_parameter type. *)
open Identifier
open Type

(** Represents type parameters. *)
type type_parameter
[@@deriving (show, sexp)]

(** Construct a type parameter. *)
val make_typaram : identifier * universe * qident * sident list -> type_parameter

(** The type parameter's name. *)
val typaram_name : type_parameter -> identifier

(** The type parameter's universe. *)
val typaram_universe : type_parameter -> universe

(** The name of the declaration this type parameter belongs to. *)
val typaram_source : type_parameter -> qident

(** The type parameter's constraints, i.e., a list of the names of typeclasses
    arguments to this type must implement. *)
val typaram_constraints : type_parameter -> sident list

(** Transform a type parameter into an equivalent type variable. *)
val typaram_to_tyvar : type_parameter -> type_var

(** Transform a type variable into an equivalent type parameter. *)
val tyvar_to_typaram : type_var -> type_parameter
