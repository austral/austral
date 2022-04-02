open Env
open TypeStripping
open MonoType2
open Tast
open Mtast

val monomorphize_ty : env -> stripped_ty -> (mono_ty * env)

val monomorphize_expr : env -> texpr -> (mexpr * env)
