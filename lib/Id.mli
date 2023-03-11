(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module contains opaque types for IDs used by the environment.

The reason this is defined outside the `Env` module is to avoid a cyclic
dependency (`Env` depends on `Tast` and `Tast` depends on IDs). *)

(** The type of file IDs. *)
type file_id
[@@deriving eq]

(** The type of module IDs. *)
type mod_id = ModId of int
[@@deriving (eq, sexp)]

(** The type of declaration IDs. *)
type decl_id = DeclId of int
[@@deriving (eq, show)]

(** The type of instance method IDs. *)
type ins_meth_id = InsMethId of int
[@@deriving (eq, show)]

(** The type of monomorph IDs. *)
type mono_id = MonoId of int
[@@deriving (eq, show, sexp)]

val fresh_file_id : unit -> file_id
val fresh_mod_id : unit -> mod_id
val fresh_decl_id : unit -> decl_id
val fresh_ins_meth_id : unit -> ins_meth_id
val fresh_mono_id : unit -> mono_id
