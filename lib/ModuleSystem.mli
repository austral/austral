open Identifier
open Semantic

type menv

val empty_menv : menv

val get_module : menv -> module_name -> semantic_module option

val get_decl : menv -> qident -> sem_decl option

val get_callable : menv -> module_name -> qident -> callable option
