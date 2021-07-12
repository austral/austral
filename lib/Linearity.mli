open Tast

(* Check that all linear variables defined in this statement are used
   consistently. *)
val check_linearity : tstmt -> unit

(* Check that all code in the declaration is used consistently. *)
val check_decl_linearity : typed_decl -> unit
