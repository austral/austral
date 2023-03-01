(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Serializing and deserializing ACM files. *)
open Sexplib
open AcmFile

type sexp = Sexp.t

val ser_compiled_module : compiled_module -> sexp

val par_compiled_module : sexp -> compiled_module
