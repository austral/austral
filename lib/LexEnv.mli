open Identifier
open Type

type lexenv

val empty_lexenv : lexenv

val push_var : lexenv -> identifier -> ty -> lexenv

val push_vars : lexenv -> (identifier * ty) list -> lexenv

val pop_var : lexenv -> lexenv

val get_var : lexenv -> identifier -> ty option
