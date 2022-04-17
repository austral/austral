(** This module implements a check over the TAST that a function ends with
    either a return statement or a call to `Abort`. *)
open Tast

val check_ends_in_return : typed_module -> unit
