(** This module defines the type_parameter type. *)
open Identifier
open Id
open Type

(** The place where a type parameter was defined: either a declaration or an instance method. *)
type typaram_source =
  | DeclSource of decl_id
  | MethodSource of ins_meth_id

(** Represents type parameters. *)
type type_parameter
[@@deriving (show, sexp)]

(** Construct a type parameter. *)
val make_typaram : identifier * universe * typaram_source * sident list -> type_parameter

(** The type parameter's name. *)
val typaram_name : type_parameter -> identifier

(** The type parameter's universe. *)
val typaram_universe : type_parameter -> universe

(** The place where this type parameter was defined. *)
val typaram_source : type_parameter -> typaram_source

(** The type parameter's constraints, i.e., a list of the names of typeclasses
    arguments to this type must implement. *)
val typaram_constraints : type_parameter -> sident list

(** Transform a type parameter into an equivalent type variable. *)
val typaram_to_tyvar : type_parameter -> type_var

(** An unsourced type parameter is like a type parameter except it's not yet
    linked to the declaration that defines it. *)
type unsourced_typaram

(** Create an unsourced type parameter. *)
val make_unsourced_typaram : identifier * universe * sident list -> unsourced_typaram

(** Given an unsourced typaram, and a typaram source, return a type parameter that links to the source. *)
val link_typaram : unsourced_typaram -> typaram_source -> type_parameter
