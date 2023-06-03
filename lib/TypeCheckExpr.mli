(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
(** Type checking of expressions. *)
open Stages.AstDB
open Tast
open Type
open Identifier
open Env
open RegionMap
open TypeParameters
open LexEnv
open TypeBindings

(** The type checking context. *)
type expr_ctx

val make_ctx : module_name -> env -> region_map -> typarams -> lexenv -> expr_ctx

(** Type check an expression. The second argument is the asserted type of the
    expression, this is used when type-checking `let` statements or `cast`
    expressions. *)
val augment_expr : expr_ctx -> ty option -> aexpr -> texpr

val augment_slot_accessor_elem : expr_ctx -> identifier -> qident -> ty list -> ty -> typed_path_elem

val augment_pointer_slot_accessor_elem : expr_ctx -> identifier -> ty -> typed_path_elem

val augment_reference_slot_accessor_elem : expr_ctx -> identifier -> qident -> ty list -> ty -> typed_path_elem

val cast_arguments : type_bindings -> value_parameter list -> texpr list -> texpr list
