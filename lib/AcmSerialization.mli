(** Serializing and deserializing ACM files. *)
open Sexplib
open AcmFile

type sexp = Sexp.t

val ser_compiled_module : compiled_module -> sexp

val par_compiled_module : sexp -> compiled_module
