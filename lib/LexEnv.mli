open Identifier
open Type

type lexenv

val empty_lexenv : lexenv

val push_var : lexenv -> identifier -> ty -> lexenv

val pop_var : lexenv -> lexenv

val get_var : lexenv -> identifier -> ty option
