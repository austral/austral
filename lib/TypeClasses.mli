(** This module implements the checking of various typeclass rules. *)
open Id
open Identifier
open Type
open TypeParameters
open Env
open EnvTypes

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

(** Given a list of instances of a given typeclass in a module, and a type
    argument, check if an instance with that type argument would overlap with
    any instance from the list. *)
val check_instance_locally_unique : decl list -> ty -> unit

(** Given the ID of the module an instance is defined in, the ID of the module
    the typeclass being implemented is from, and the argument type, check the
    orphan rules.

    A typeclass is local if it is defined in this module, and foreign
    otherwise. A type is local if it is defined in this module, or if it is a
    builtin type, and is foreign otherwise.

    The orphan rules are:

        1. Local typeclass and local type = ok
        2. Local typeclass and foreign type = ok
        3. Foreign typeclass and local type = ok
        4. Foreign typeclass and foreign type = bad
*)
val check_instance_orphan_rules : env -> mod_id -> mod_id -> ty -> unit

(** We don't allow any of the type parameters of a generic instance to have the
    same name as the type parameter of the corresponding typeclass.

  This is a hack to get around: https://github.com/austral/austral/issues/244

  But this might, I suspect, become part of the language because it reduces
  confusion. *)
val check_disjoint_typarams : identifier -> typarams -> unit
