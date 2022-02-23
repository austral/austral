open Identifier
open Type
open Region
open Ast
open Env

(* Given a type's list of type parameters, its declared universe, and a list of
   supplied type arguments, evaluate the effective universe the type belongs to.

   Preconditions: the lists have the same length. *)
val effective_universe : qident -> type_parameter list -> universe -> ty list -> universe

val any_arg_is_linear : ty list -> bool

val any_arg_is_type : ty list -> bool

(* Find the type signature of the type with the given name, if any
   exists. Searches the local type signature list first. *)
val get_type_signature : env -> type_signature list -> qident -> type_signature option

(* Parse a qualified type specifier.

   The second argument is the list of local type signatures from the module the
   type specifier is being parsed in.Arith_status

   The third argument is the list of type parameters known at parse time.  *)
val parse_type : env -> type_signature list -> region_map -> type_parameter list -> qtypespec -> ty

val universe_compatible : universe -> universe -> bool
