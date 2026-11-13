(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type
open TypeSignature
open TypeParameters
open RegionMap
open Stages.Ast
open Env

(* Parse a qualified type specifier.

   The second argument is the list of local type signatures from the module the
   type specifier is being parsed in.Arith_status

   The third argument is the list of type parameters known at parse time.  *)
val parse_type : env -> type_signature list -> region_map -> typarams -> qtypespec -> ty

val universe_compatible : universe -> universe -> bool
