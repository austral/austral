(** Serializing and deserializing ACM files. *)
open Sexplib
open AcmFile
open Env

type sexp = Sexp.t

val ser_compiled_module : env -> compiled_module -> sexp
