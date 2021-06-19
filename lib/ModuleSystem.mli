open Identifier
open Semantic

type menv

val empty_menv : menv

val get_decl : menv -> qident -> sem_decl option

val get_callable : menv -> module_name -> qident -> callable option
