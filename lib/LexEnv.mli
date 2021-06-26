open Identifier
open Type

type lexenv

val empty_lexenv : lexenv

val push_scope : lexenv -> lexenv

val pop_scope : lexenv -> lexenv

val get_var : lexenv -> identifier -> ty option

val add_var : lexenv -> identifier -> ty -> lexenv
