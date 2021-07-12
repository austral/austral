open Identifier
open Tast

(* Check that a linear variable is used consistently in the body in which it is
   defined. *)
val check_consistency : identifier -> tstmt -> unit
