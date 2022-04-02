open Env
open TypeStripping
open MonoType2

val monomorphize_ty : env -> stripped_ty -> (mono_ty * env)
