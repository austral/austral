(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors in the structure of declarations, and their wording. *)
open ErrorText

type t =
  | ForeignInSafeModule
  | ForeignTypeParameters
  | FunInvalidPragmas

let render (error: t): err_text =
  match error with
  | ForeignInSafeModule ->
     [
       Text "Can't declare a foreign function in a safe module."
     ]
  | ForeignTypeParameters ->
     [
       Text "Foreign functions can't have type parameters."
     ]
  | FunInvalidPragmas ->
     [
       Text "Only import and export pragmas are allowed inside functions."
     ]
