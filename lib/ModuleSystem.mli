open Identifier
open Semantic
open LexEnv
open Type

type menv

val empty_menv : menv

val put_module : menv -> semantic_module -> menv

val get_module : menv -> module_name -> semantic_module option

val get_decl : menv -> qident -> sem_decl option

(* The second argument is the name of the module where typechecking is taking place. *)
val get_callable : menv -> module_name -> qident -> callable option

(** Get a variable either from a lexenv or from a constant. *)
val menv_get_var : menv -> lexenv -> qident -> ty option
