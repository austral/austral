(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Stores the lexical environment. *)
open Identifier
open Type

(** A lexical environment. *)
type lexenv

(** Where an identifier comes from: a constant, a function parameter, or a local
    variable. *)
type var_source =
  | VarConstant
  | VarParam
  | VarLocal

(** The empty lexenv. *)
val empty_lexenv : lexenv

(** Add a variable to the lexenv given its name, type, and source. *)
val push_var : lexenv -> identifier -> ty -> var_source -> lexenv

(** Add a list of variables. *)
val push_vars : lexenv -> (identifier * ty * var_source) list -> lexenv

(** Remove the last variable added. *)
val pop_var : lexenv -> lexenv

(** Retrieve a variable by name. *)
val get_var : lexenv -> identifier -> (ty * var_source) option
