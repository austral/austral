(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The linearity errors, and their wording. *)
open Type
open ErrorText

type t =
  | DereferenceNonFree of ty

let render (error: t): err_text =
  match error with
  | DereferenceNonFree ty ->
     [
       Text "Cannot dereference non-free type ";
       Type ty
     ]
