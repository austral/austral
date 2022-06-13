(** Serializing and deserializing ACM files. *)
open Sexplib
open AcmFile

type sexp = Sexp.t

val ser_acm_type_vis : acm_type_vis -> sexp
val par_acm_type_vis : sexp -> acm_type_vis
