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
