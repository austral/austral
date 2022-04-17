(** This module implements a check over the TAST that a function ends with
    either a return statement or a call to `Abort`. *)
open Combined

val check_ends_in_return : combined_module -> unit
