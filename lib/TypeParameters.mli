(** This module implements an ordered set of type parameters. This is used to
    store the type parameter list in declarations. *)
open Identifier
open Type

(** An ordered set of type parameters keyed by the parameter's name. *)
type typarams
[@@deriving (show, sexp)]

(** An empty type parameter set. *)
val empty_typarams : typarams

(** The number of type parameters in the set. *)
val typarams_size : typarams -> int

(** Find a type parameter by name. *)
val get_typaram : typarams -> identifier -> type_parameter option

(** Add a type parameter to the last position in the set. Throws an error if a
    type parameter with this name already exists. *)
val add_typaram : typarams -> type_parameter -> typarams

(** Return the type parameters as a list, preserving order. *)
val typarams_as_list : typarams -> type_parameter list

(** Convert a list into a type parameter set, checking uniqueness. *)
val typarams_from_list : type_parameter list -> typarams
