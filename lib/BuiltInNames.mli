(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Every string that names something built in: the built-in types, the
    built-in modules, and the declarations of those modules that the compiler
    refers to. No other module spells such a name as a string literal.

    Use these names qualified. Opening this module would shadow the
    constructors of [Type.ty]. *)

open Common

(* Types *)

(** The types the type parser recognises by their name. *)
type builtin_type =
  | Unit
  | Bool
  | Nat8
  | Nat16
  | Nat32
  | Nat64
  | Int8
  | Int16
  | Int32
  | Int64
  | ByteSize
  | Index
  | Float32
  | Float64
  | Static
  | Fn

(** The name a built-in type is written with. *)
val type_name : builtin_type -> string

(** The built-in type that is written with this name. *)
val type_of_name : string -> builtin_type option

(** The type constructors that have their own syntax, as a type is shown to
    the user. *)
val read_ref_name : string
val write_ref_name : string
val span_name : string
val span_mut_name : string

(* Modules *)

val pervasive_module : string
val memory_module : string

(** The module the monomorphs of exported functions are generated in, and the
    C unit that holds the wrappers of those functions. *)
val wrappers_module : string
val wrappers_unit_name : string

(* Austral.Memory *)

val address_name : string
val pointer_name : string

(* Austral.Pervasive *)

val abort_name : string
val root_capability_name : string
val exit_code_name : string
val exit_success_name : string
val exit_failure_name : string

(** The method an arithmetic operator is a call to. *)
val arithmetic_method_name : arithmetic_operator -> string

(** The names every module imports from [Austral.Pervasive], in the order
    they are imported. *)
val pervasive_import_names : string list

(* Pragmas *)

val foreign_import_pragma_name : string
val foreign_export_pragma_name : string
val unsafe_module_pragma_name : string

(** The one named argument of the two foreign pragmas. *)
val external_name_argument : string

(* Names the compiler makes up *)

(** A function pointer type has no parameter names. The type checker gives
    every parameter this one. *)
val fake_parameter_name : string
