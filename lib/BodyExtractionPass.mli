open Env
open Tast

(** Given a typed module, extract the function and instance method bodies and
    store them in the environment. *)
val extract_bodies : env -> typed_module -> env
