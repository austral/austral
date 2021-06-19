open Identifier
open Type
open Ast
open ModuleSystem

val is_built_in_type : string -> ty option

(* Given a type's list of type parameters, its declared universe, and a list of
   supplied type arguments, evaluate the effective universe the type belongs to.

   Preconditions: the lists have the same length. *)
val effective_universe : type_parameter list -> universe -> ty list -> universe

(* Find the type signature of the type with the given name, if any
   exists. Searches the local type signature list first. *)
val get_type_signature : menv -> type_signature list -> qident -> type_signature option

(* Parse a qualified type specifier.

   The first argument is the list of type parameters known at parse time. The
   second argument is the type specifier to parse. *)
val parse : type_parameter list -> qtypespec -> ty
