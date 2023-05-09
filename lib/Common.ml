(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Simple types used throughout the codebase. *)

(** The four arithmetic operators. *)
type arithmetic_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
[@@deriving (show, sexp)]

(** The comparison operators. *)
type comparison_operator =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
[@@deriving (show, sexp)]

(** The type of documentation strings. *)
type docstring = Docstring of string

(** The type of declaration pragmas. *)
type pragma =
  | ForeignImportPragma of string
  | ForeignExportPragma of string
  | UnsafeModulePragma

(** Whether a variable is immutable or mutable. *)
type mutability =
  | Immutable
  | Mutable
[@@deriving (show)]

(** Whether a reference is a read reference or a write reference. *)
type borrowing_mode =
  | ReadBorrow
  | WriteBorrow
[@@deriving (show, sexp)]

(** Whether a module is safe or unsafe. *)
type module_kind =
  | SafeModule
  | UnsafeModule

(** The visibility of a type: public, opaque, or private. *)
type type_vis =
  | TypeVisPublic
  | TypeVisOpaque
  | TypeVisPrivate

(** The visibility of another declaration: public or private. *)
type vis =
  | VisPublic
  | VisPrivate
