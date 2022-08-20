(** This module defines a type for type bindings: a map from type parameters to
    types. *)
open Type
open TypeParameter
open Sexplib

(** A type bindings value represents a map from type parameters to actual
    types. These are created as part of the type checking process. *)
type type_bindings

val pp_type_bindings : Format.formatter -> type_bindings -> unit

val show_type_bindings : type_bindings -> string

val binding_count : type_bindings -> int

val bindings_list : type_bindings -> (type_parameter * ty) list

val empty_bindings : type_bindings

val bindings_from_list : (type_parameter * ty) list -> type_bindings

val merge_bindings : type_bindings -> type_bindings -> type_bindings

val add_binding : type_bindings -> type_parameter -> ty -> type_bindings

val get_binding : type_bindings -> type_parameter -> ty option

(* Given a set of bindings, and a type expression, replace every type variable
   in the expression with the value from the bindings. If the variable is not
   found in the bindings, do nothing. *)
val replace_variables : type_bindings -> ty -> ty

val show_bindings : type_bindings -> string

val type_bindings_of_sexp : Sexp.t -> type_bindings
val sexp_of_type_bindings : type_bindings -> Sexp.t
