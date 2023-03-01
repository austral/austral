(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Imports
open Cst
open Ast

(* Turn an identifier into a qualified identifier, given the import map. *)
val qualify_identifier : import_map -> identifier -> qident

val qualify_typespec : import_map -> typespec -> qtypespec
