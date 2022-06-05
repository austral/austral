(** This module implements the checking of various typeclass rules. *)
open Type
open TypeParameters

(** Given the universe a typeclass accepts types from, and the argument to an
    instance of that typeclass, check the argument's universe is acceptable. *)
val check_instance_argument_has_right_universe : universe -> ty -> unit

(** Given the set of type parameters of a generic instance, and the instance's
    argument type, check the argument has the right shape. That is, it is
    either:

    1. A concrete type.

    2. A generic type applid to a set of *distinct* type variables, which are
       all the variables in the type parameter set.

 *)
val check_instance_argument_has_right_shape : typarams -> ty -> unit

(** Given the argument types to two instances of the same typeclass, check
    whether they overlap. *)
val overlapping_instances : ty -> ty -> bool
