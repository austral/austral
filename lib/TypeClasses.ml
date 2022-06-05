open Id
open Identifier

(** The argument to a typeclass is either a concrete type applied to zero
    arguments, or a generic type applied to a number of type variables. *)
type instance_argument =
  InstanceArgument of decl_id * identifier list
