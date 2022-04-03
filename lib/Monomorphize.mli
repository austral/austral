open Env
open TypeStripping
open MonoType2
open Tast
open Mtast

(** Monomorphize a type specifier, returning a monomorphic type and the updated
   environment. *)
val monomorphize_ty : env -> stripped_ty -> (mono_ty * env)

(** Monomorphize an expression, returning a monomorphic expression and the
   updated environment. *)
val monomorphize_expr : env -> texpr -> (mexpr * env)

(** Monomorphize a statement, returning a monomorphic statement and the updated
   environment. *)
val monomorphize_stmt : env -> tstmt -> (mstmt * env)

(** Monomorphize a declaration, returning a monomorphic declaration and the
   updated environment. Return {None} where a declaration has no immediate
   monomorph (e.g.: generic type definitions, which are monomorphized on
   demand). *)
val monomorphize_decl : env -> typed_decl -> (mdecl option * env)

(** This is the entrypoint to the monomorphization pass.

    Monomorphizing a module involves:

    1. Take all the declarations and monomorphize the ones you can (e.g.:
   concrete function definitions, concrete records, concrete instances).

    2. In the process, monomorphs will be collected into the environment.

    3. Get all uninstantiated monomorphs from the environment, and instantiate
   them. Again, new (uninstantiated) monomorphs will be collected and added to
   the environment: for example, if you instantiate a generic function `f` and
   the body of that function contains a call to a generic function `g` with
   monomorphic arguments, that monomorph of `g` will be added to the
   environment.

    4. Repeat step #3 until there are no more uninstantiated monomorphs in the
   environment.

    5. You're done.

*)
val monomorphize : env -> typed_module -> (env * mono_module)
