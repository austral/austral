(** Stores the lexical environment. *)
open Identifier
open Type

type lexenv

type var_source =
  | VarConstant
  | VarParam
  | VarLocal

val empty_lexenv : lexenv

val push_var : lexenv -> identifier -> ty -> var_source -> lexenv

val push_vars : lexenv -> (identifier * ty * var_source) list -> lexenv

val pop_var : lexenv -> lexenv

val get_var : lexenv -> identifier -> (ty * var_source) option
